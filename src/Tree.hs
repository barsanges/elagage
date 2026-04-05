{- |
   Module      : Tree
   Copyright   : Copyright (C) 2026 barsanges

Le livre représenté sous la forme d'un arbre non recombinant.
-}

module Tree
  ( module Paragraph
  , module Raw
  , Tree(..)
  , raw2tree
  , getRootParagraph
  ) where

import qualified Data.Map as M

import Paragraph
import Raw

-- | Le livre représenté sous la forme d'un arbre non recombinant.
data Tree = Branch Paragraph Status [Tree]
          | Leaf Paragraph Status
  deriving (Show, Eq)

-- | Obtient le paragraphe de la racine d'un arbre.
getRootParagraph :: Tree -> Paragraph
getRootParagraph (Branch paragraph _ _) = paragraph
getRootParagraph (Leaf paragraph _) = paragraph

-- | Obtient le statut de la racine d'un arbre.
getRootStatus :: Tree -> Status
getRootStatus (Branch _ status _) = status
getRootStatus (Leaf _ status) = status

-- | Change le status de la racine d'un arbre.
setRootStatus :: Tree -> Status -> Tree
setRootStatus (Branch p _ ts) status = Branch p status ts
setRootStatus (Leaf p _) status = Leaf p status

-- | Met la partie explorée du livre sous forme d'arbre non recombinant. Le
-- premier paragraphe a le numéro 1.
raw2tree ::  Raw -> Tree
raw2tree raw = setRootStatus (go [] (Paragraph 1)) Solution
    where
      -- go :: [Paragraph] -> Paragraph -> Tree
      go previous current = case M.lookup current raw of
        Nothing -> Leaf current Unknown
        Just comp -> og previous current comp

      -- og :: [Paragraph] -> Paragraph -> Component -> Tree
      og previous current comp
          | eval (final_ comp) previous = Leaf current Solution
          | eval (deadEnd_ comp) previous = Leaf current DeadEnd
          -- On s'arrête dès que le chemin est un cycle : de manière
          -- erronée, on traitera donc comme des culs-de-sac des
          -- chemins où, pour aboutir à la solution, il *faudrait*
          -- passer plusieurs fois par les mêmes paragraphes.
          | current `elem` previous = Leaf current DeadEnd
          | null neighbors = Leaf current DeadEnd
          | otherwise = Branch current status subtrees
        where
          neighbors = arrows_ comp
          subtrees = fmap (go (current:previous)) neighbors
          status = maximum $ fmap getRootStatus subtrees
