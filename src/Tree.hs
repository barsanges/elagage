{- |
   Module      : Tree
   Copyright   : Copyright (C) 2026 barsanges

Le livre représenté sous la forme d'un arbre non recombinant.
-}

module Tree
  ( module Paragraph
  , Tree(..)
  , raw2tree
  , getRootParagraph
  ) where

import qualified Data.Map as M

import Paragraph

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

-- | Met la partie explorée du livre sous forme d'arbre non recombinant.
raw2tree :: Paragraph                   -- ^ Le premier paragraphe du livre
         -> M.Map Paragraph [Paragraph] -- ^ Les correspondances connues (devrait moralement être `M.Map Paragraph (S.Set Paragraph)`)
         -> M.Map Paragraph Criteria    -- ^ Les culs-de-sac déjà identifiés
         -> M.Map Paragraph Criteria    -- ^ Les fins déjà identifiées
         -> Tree                        -- ^ La part explorée du livre mise sous forme d'arbre
raw2tree start arrows deadEnds finals = setRootStatus (go [] start) Solution
    where
      -- go :: [Paragraph] -> Paragraph -> Tree
      go previous current
        | eval (M.findWithDefault Never current finals) previous = Leaf current Solution
        | eval (M.findWithDefault Never current deadEnds) previous = Leaf current DeadEnd
        -- On s'arrête dès que le chemin est un cycle : de manière erronée, on
        -- traitera donc comme des culs-de-sac des chemins où, pour aboutir à
        -- la solution, il *faudrait* passer plusieurs fois par les mêmes
        -- paragraphes.
        | current `elem` previous = Leaf current DeadEnd
        | otherwise = case M.lookup current arrows of
            Nothing -> Leaf current Unknown
            Just neighbors -> Branch current status subtrees
              where
                subtrees = fmap (go (current:previous)) neighbors
                status = maximum $ fmap getRootStatus subtrees
