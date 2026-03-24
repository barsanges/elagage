{- |
   Module      : Paragraph
   Copyright   : Copyright (C) 2026 barsanges

Les paragraphes du livre et leur statut dans l'exploration.
-}

module Paragraph
  ( Paragraph(..)
  , Status(..)
  , Conditions(..)
  , Criteria(..)
  , eval
  ) where

import qualified Data.Set as S

-- | Un numéro de paragraphe.
newtype Paragraph = Paragraph Int
  deriving (Show, Eq, Ord)

-- | Le statut d'un paragraphe : permet-il d'atteindre la solution ?
data Status = Solution
            | Unknown
            | DeadEnd
  deriving (Show, Eq)

instance Ord Status where
  compare DeadEnd DeadEnd = EQ
  compare DeadEnd Unknown = LT
  compare DeadEnd Solution = LT

  compare Unknown DeadEnd = GT
  compare Unknown Unknown = EQ
  compare Unknown Solution = LT

  compare Solution DeadEnd = GT
  compare Solution Unknown = GT
  compare Solution Solution = EQ

-- | Des conditions de passage pour déterminer le statut d'un paragraphe.
data Conditions = Conditions { hasNone :: S.Set Paragraph
                               -- ^ Il ne faut passer par aucun de ces paragraphes
                             , hasAll :: [S.Set Paragraph]
                               -- ^ Il faut être passé par au moins un paragraphe de chaque groupe
                             }

-- | Des critères pour déterminer le statut d'un paragraphe.
data Criteria = Always
              | Never
              | Sometimes Conditions

-- | Indique si l'intersection de deux sets est non vide.
intersect :: Ord a => S.Set a -> S.Set a -> Bool
intersect x y = not $ S.disjoint x y

-- | Indique si les critères éventuels sont vérifiés, en évaluant les
-- conditions s'il y en a. Renvoie `False` si le critère est `Never`,
-- `True` si le critère est `Always`, et évalue les conditions dans
-- les autres cas.
eval :: Criteria -> [Paragraph] -> Bool
eval Never _ = False
eval Always _ = True
eval (Sometimes cond) xs = (and needed) && excluded
  where
    ys = S.fromList xs
    needed = fmap (intersect ys) (hasAll cond)
    excluded = S.disjoint (hasNone cond) ys
