{- |
   Module      : Main
   Copyright   : Copyright (C) 2026 barsanges

Point d'entrée du programme.
-}

module Main where

import Data.Aeson
import qualified Data.ByteString as BS
import Options.Applicative

import Diagram

-- | Le type d'entrée.
data Input =  InputFile String
            | StdIn

-- | Le type de sortie.
data Output = OutputFile String
            | StdOut

data Args = Args
  { input :: Input
  , output :: Output
  }

argsParser :: Parser Args
argsParser = Args
  <$> ( (InputFile <$> strOption
         ( long "fin"
           <> short 'i'
           <> metavar "FIN"
           <> help "Process the input from the file 'FIN' instead of taking it from stdin"
         ))
        <|> pure StdIn
      )
  <*> ( (OutputFile <$> strOption
         ( long "fout"
           <> short 'o'
           <> metavar "FOUT"
           <> help "Write the result to the file 'FOUT' instead of sending it to stdout"
         ))
        <|> pure StdOut
      )

-- | Le parser de la ligne de commande pour 'elagage'.
args :: ParserInfo Args
args = info (argsParser <**> helper)
  ( fullDesc
  <> header "elagage"
  <> progDesc "Draw the connections between the known paragraphs of a gamebook" )

-- | Point d'entrée du programme.
main :: IO ()
main = do
  cli <- execParser args
  mraw <- case input cli of
            InputFile f -> eitherDecodeFileStrict f
            StdIn -> fmap eitherDecodeStrict BS.getContents
  case mraw of
    Left msg -> putStrLn msg
    Right raw -> let res = (graph2mermaid . tree2graph . raw2tree) raw
                 in case output cli of
                      OutputFile f -> writeFile f res
                      StdOut -> putStrLn res
