{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Raw
   Copyright   : Copyright (C) 2026 barsanges

Les données connues à date pour un livre.
-}

module Raw
  ( module Paragraph
  , Raw
  , json2raw
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import Data.Either ( partitionEithers )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read ( readMaybe )

import Paragraph

-- | Les données connues à date pour un livre.
type Raw = M.Map Paragraph (S.Set Paragraph)

-- | Transforme une clef Aeson en chaîne de caractères.
key2string :: Key -> String
key2string = T.unpack . AK.toText

-- | Transforme des données au format JSON en un objet `Raw`.
json2raw :: Value -> Either String Raw
json2raw (Array _) = Left "expecting an object as JSON data, got an array"
json2raw (String _) = Left "expecting an object as JSON data, got a string"
json2raw (Number _) = Left "expecting an object as JSON data, got a number"
json2raw (Bool _) = Left "expecting an object as JSON data, got a boolean"
json2raw Null = Left "expecting an object as JSON data, got null"
json2raw (Object root) = AKM.foldrWithKey go (Right M.empty) root
  where
    -- go :: Key -> Value -> Either String Raw -> Either String Raw
    go _ _ (Left msg) = Left msg -- FIXME : on pourrait améliorer pour parser toutes les erreurs d'un coup
    go key (Array _) _ = Left ("for key '" ++ (key2string key) ++ "': expected an object, got an array")
    go key (String _) _ = Left ("for key '" ++ (key2string key) ++ "': expected an object, got a string")
    go key (Number _) _ = Left ("for key '" ++ (key2string key) ++ "': expected an object, got a number")
    go key (Bool _) _ = Left ("for key '" ++ (key2string key) ++ "': expected an object, got a boolean")
    go key Null _ = Left ("for key '" ++ (key2string key) ++ "': expected an object, got null")
    go key (Object sub) (Right raw) = case AKM.lookup (AK.fromText "correspondances") sub of
      Nothing -> Left ("for key '" ++ key' ++ "': missing field 'correspondances'")
      Just (String _) -> Left ("for key '" ++ key' ++ "/correspondances': expected an array, got a string")
      Just (Number _) -> Left ("for key '" ++ key' ++ "/correspondances': expected an array, got a number")
      Just (Bool _) -> Left ("for key '" ++ key' ++ "/correspondances': expected an array, got a boolean")
      Just Null -> Left ("for key '" ++ key' ++ "/correspondances': expected an array, got null")
      Just (Object _) -> Left ("for key '" ++ key' ++ "/correspondances': expected an array, got an object")
      Just (Array correspondances) -> do
        content <- og correspondances
        paragraph <- case (readMaybe key') :: (Maybe Int) of
          Nothing -> Left ("for key '" ++ key' ++ "': the key cannot be parsed as an integer")
          Just n -> Right (Paragraph n)
        return (M.insert paragraph content raw)
      where
        key' = key2string key

        -- og :: V.Vector Value -> Either String (S.Set Paragraph)
        og xs = if lefts == []
                then Right (S.fromList rights)
                else Left (unlines lefts)
          where
            (lefts, rights) = partitionEithers (V.toList (V.imap helper xs))

        -- helper :: Int -> Value -> Either String Paragraph
        helper i (String x) = case (readMaybe (T.unpack x)) :: (Maybe Int) of
                                Nothing -> Left ("for key '" ++ key' ++ "/correspondances': unable to parse the element " ++ (show i) ++ " of the array as a paragraph number")
                                Just n -> Right (Paragraph n)
        helper i _ = Left ("for key '" ++ key' ++ "/correspondances': unable to parse the element " ++ (show i) ++ " of the array as a paragraph number")
