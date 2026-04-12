{- |
   Module      : Diagram
   Copyright   : Copyright (C) 2026 barsanges

Le livre représenté sous la forme d'un diagramme.
-}

module Diagram
  ( module Graph
  , graph2mermaid
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Graph

-- | Met le grapĥe sous la forme d'instructions Mermaid.
graph2mermaid :: Graph -> String
graph2mermaid graph = if M.null (vertices_ graph)
                      then ""
                      else unlines (reverse result)
  where
    preamble = [ "  classDef solution fill:#008000,color:#ffffff,stroke:#000000"
               , "  classDef unknown fill:#DCDCDC,stroke:#000000"
               , "  classDef deadEnd fill:#CD5C5C,color:#ffffff,stroke:#000000"
               , "flowchart TD"
               ]
    (result, _) = M.foldlWithKey' go1 (preamble, S.empty) (vertices_ graph)

    go1 :: ([String], S.Set Int) -> Paragraph -> S.Set Paragraph -> ([String], S.Set Int)
    go1 acc (Paragraph p) arrows = S.foldl' (go2 p) (defineNode acc p) arrows

    go2 :: Int -> ([String], S.Set Int) -> Paragraph -> ([String], S.Set Int)
    go2 p acc (Paragraph q) = (text', seen)
      where
        (text, seen) = defineNode acc q
        text' = ("  n" ++ (show p) ++ " --> n" ++ (show q)):text

    defineNode :: ([String], S.Set Int) -> Int -> ([String], S.Set Int)
    defineNode (text, seen) p = (text', seen')
      where
        text' = if (p `S.member` seen)
                then text
                else case M.lookup (Paragraph p) (nodes_ graph) of
                       Just status -> case status of
                         Solution -> ("  n" ++ (show p) ++ "[" ++ (show p) ++ "]:::solution"):text
                         Unknown -> ("  n" ++ (show p) ++ "(" ++ (show p) ++ "):::unknown"):text
                         DeadEnd -> ("  n" ++ (show p) ++ "[" ++ (show p) ++ "]:::deadEnd"):text
                       Nothing -> ("  n" ++ (show p) ++ "(" ++ (show p) ++ "):::unknown"):text
        seen' = S.insert p seen
