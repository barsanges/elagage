{- |
   Module      : Paragraph
   Copyright   : Copyright (C) 2026 barsanges

Les paragraphes du livre et leur statut dans l'exploration.
-}

module Paragraph
  ( Paragraph(..)
  , Status(..)
  ) where

-- | Un numéro de paragraphe.
newtype Paragraph = Paragraph Int
  deriving (Show, Eq, Ord)

-- | Le statut d'un paragraphe : permet-il d'atteindre la solution ?
data Status = Solution
            | Unknown
            | DeadEnd
            | Unexplored
  deriving (Show, Eq)
