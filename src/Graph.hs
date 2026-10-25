{- |
   Module      : Graph
   Copyright   : Copyright (C) 2026 barsanges

Le livre représenté sous la forme d'un graphe orienté.
-}

module Graph
  ( module Paragraph
  , module Raw
  , Graph(..)
  , raw2graph
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Paragraph
import Raw

-- | Le livre représenté sous la forme d'un graphe orienté.
data Graph = Graph { nodes_ :: M.Map Paragraph Status
                   , edges_ :: M.Map Paragraph (S.Set Paragraph)
                   }
  deriving (Show, Eq)

-- | Le livre représenté sous la forme d'un graphe orienté.
data WorkingGraph = WorkingGraph { vertices_ :: M.Map Paragraph Status
                                 , ancestors_ :: M.Map Paragraph (S.Set Paragraph)
                                 , descendants_ :: M.Map Paragraph (S.Set Paragraph)
                                 }
  deriving (Show, Eq)

-- | Un graphe vide.
empty :: WorkingGraph
empty = WorkingGraph { vertices_ = M.empty
                     , ancestors_ = M.empty
                     , descendants_ = M.empty
                     }

-- | Ajoute un sommet au graphe.
addVertex :: Paragraph -> Status -> S.Set Paragraph -> WorkingGraph -> WorkingGraph
addVertex p status arrows graph = WorkingGraph { vertices_ = vertices'
                                               , ancestors_ = ancestors'
                                               , descendants_ = descendants'
                                               }
  where
    vertices' = M.insert p status (vertices_ graph)
    descendants' = M.insert p arrows (descendants_ graph)
    ancestors' = S.foldr go (ancestors_ graph) arrows

    -- go :: Paragraph -> M.Map Paragraph (S.Set Paragraph) -> M.Map Paragraph (S.Set Paragraph)
    go q ancestorsMap = case M.lookup q ancestorsMap of
      Just knownAncestors -> M.insert q (S.insert p knownAncestors) ancestorsMap
      Nothing -> M.insert q (S.singleton p) ancestorsMap

-- | Initialise le graphe à partir des données connues à date pour le
-- livre. Les noeuds sont par défaut au statut `Unknown`, sauf ceux
-- qui sont inconditionnellement des solutions ou des culs de sac.
initializeGraph :: Raw -> WorkingGraph
initializeGraph raw = M.foldrWithKey go empty raw
  where
    -- go :: Paragraph -> S.Set Paragraph -> Graph -> Graph
    go p arrows graph = addVertex p Unknown arrows graph

-- | Marque les noeuds qui n'ont jamais été explorés.
tagUnexplored :: WorkingGraph -> WorkingGraph
tagUnexplored graph = foldr go graph unexplored
  where
    visitedVertices = M.keysSet (vertices_ graph)
    knownVertices = S.unions (M.elems (descendants_ graph))
    unexplored = S.difference knownVertices visitedVertices

    -- go :: Paragraph -> Graph -> Graph
    go p graph' = addVertex p Unexplored S.empty graph'

-- | Renvoie l'ensemble des descendants (i.e. : à toutes les générations) de
-- chaque noeud du graphe.
findAllDescendants :: WorkingGraph -> M.Map Paragraph (S.Set Paragraph)
findAllDescendants wgraph = M.fromList [(p, go S.empty (sons p)) | p <- allVertices]
  where
    allVertices = M.keys (vertices_ wgraph)

    -- sons :: Paragraph -> S.Set Paragraph
    sons p = M.findWithDefault S.empty p (descendants_ wgraph)

    -- go :: S.Set Paragraph -> S.Set Paragraph -> S.Set Paragraph
    go old current = if S.null new
                     then current
                     else go current newest
      where
       new = S.difference current old
       newest = S.union current (S.unions (fmap sons (S.toList new)))

-- | Marque comme culs-de-sac les noeuds qui ne permettent pas
-- d'atteindre un noeud non exploré.
tagObviousDeadEnds :: WorkingGraph -> WorkingGraph
tagObviousDeadEnds wgraph = wgraph { vertices_ = vertices' }
  where
    allDescendants = findAllDescendants wgraph
    vertices' = M.foldrWithKey go (vertices_ wgraph) allDescendants

    -- go :: Paragraph -> S.Set Paragraph -> M.Map Paragraph Status -> M.Map Paragraph Status
    go p descendants vertices = case M.lookup p vertices of
                                  Just Unknown -> if S.null (S.filter isUnexplored descendants)
                                                  then M.insert p DeadEnd vertices
                                                  else vertices
                                  _ -> vertices
      where
        -- FIXME : il faudrait aussi chercher s'il y a des noeuds de fin !
        -- isUnexplored :: Paragraph -> Bool
        isUnexplored q = case M.lookup q vertices of
          Just Unexplored -> True
          _ -> False

-- | Met la partie explorée du livre sous forme d'un graphe orienté. Le
-- premier paragraphe a le numéro 1.
raw2graph :: Raw -> Graph
raw2graph raw = Graph { nodes_ = vertices_ wgraph
                      , edges_ = descendants_ wgraph
                      }
  where
    wgraph = (tagObviousDeadEnds . tagUnexplored . initializeGraph) raw
