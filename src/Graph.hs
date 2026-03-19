{- |
   Module      : Graph
   Copyright   : Copyright (C) 2026 barsanges

Le livre représenté sous la forme d'un graphe orienté.
-}

module Graph
  ( module Tree
  , Graph(..)
  , tree2graph
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Tree

-- | Le livre représenté sous la forme d'un graphe orienté.
data Graph = Graph { nodes_ :: M.Map Paragraph Status
                   , vertices_ :: M.Map Paragraph (S.Set Paragraph)
                   }
  deriving (Show, Eq)

-- | Un graphe vide.
empty :: Graph
empty = Graph { nodes_ = M.empty, vertices_ = M.empty }

-- | Met à jour (si nécessaire) le statut d'un noeud du graphe.
updateNode :: Paragraph -> Status -> Graph -> Graph
updateNode x status graph = graph { nodes_ = M.insertWith max x status $ nodes_ graph }

-- | Met à jour (si nécessaire) les liens entre les noeuds du graphe.
updateVertices :: Paragraph -> Paragraph -> Graph -> Graph
updateVertices x y graph = graph { vertices_ = M.insertWith S.union x (S.singleton y) $ vertices_ graph }

-- | Convertit un arbre non recombinant en un graphe orienté.
tree2graph :: Tree -> Graph
tree2graph initial = goVertices Nothing empty initial
  where
    -- goVertices :: Maybe Paragraph -> Graph -> Tree -> Graph
    goVertices previous graph tree = goNode new tree
      where
        new = case previous of
          Nothing -> graph
          Just prev -> updateVertices prev (getRootParagraph tree) graph

    -- goNode :: Graph -> Tree -> Graph
    goNode graph (Leaf current status) = updateNode current status graph
    goNode graph (Branch current status subtrees) = updateNode current status new
      where
        new = foldl' (goVertices $ Just current) graph subtrees
