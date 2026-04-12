{- |
   Module      : DiagramSpec
   Copyright   : Copyright (C) 2026 barsanges

Teste le module `Diagram`.
-}

module DiagramSpec ( spec ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.Hspec
import Diagram

spec :: Spec
spec = do
  describe "graph2mermaid" $ do
    it "should return an empty string if the graph is empty" $
      (graph2mermaid (Graph { nodes_ = M.empty, vertices_ = M.empty })) `shouldBe` ""

    it "should turn a graph into a Mermaid diagram (1)" $
      (graph2mermaid (Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                  , (Paragraph 13, Unknown)
                                                  , (Paragraph 22, Unknown)
                                                  , (Paragraph 48, DeadEnd)
                                                  , (Paragraph 105, Unknown)
                                                  ]
                            , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 22
                                                                                , Paragraph 105
                                                                                ] )
                                                     , (Paragraph 22, S.fromList [ Paragraph 13
                                                                                 , Paragraph 48
                                                                                 ] )
                                                     ]
                            })) `shouldBe` "flowchart TD\n\
                                           \  classDef deadEnd fill:#CD5C5C,color:#ffffff,stroke:#000000\n\
                                           \  classDef unknown fill:#DCDCDC,stroke:#000000\n\
                                           \  classDef solution fill:#008000,color:#ffffff,stroke:#000000\n\
                                           \  n1[1]:::solution\n\
                                           \  n22(22):::unknown\n\
                                           \  n1 --> n22\n\
                                           \  n105(105):::unknown\n\
                                           \  n1 --> n105\n\
                                           \  n13(13):::unknown\n\
                                           \  n22 --> n13\n\
                                           \  n48[48]:::deadEnd\n\
                                           \  n22 --> n48\n"

    it "should turn a graph into a Mermaid diagram (2)" $
      (graph2mermaid (Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                  , (Paragraph 17, Unknown)
                                                  , (Paragraph 36, DeadEnd)
                                                  , (Paragraph 63, Solution)
                                                  , (Paragraph 88, Unknown)
                                                  , (Paragraph 100, Solution)
                                                  ]
                            , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 63
                                                                                , Paragraph 88
                                                                                ] )
                                                     , (Paragraph 36, S.fromList [ Paragraph 63
                                                                                 ] )
                                                     , (Paragraph 63, S.fromList [ Paragraph 17
                                                                                 , Paragraph 100
                                                                                 ] )
                                                     , (Paragraph 88, S.fromList [ Paragraph 17
                                                                                 , Paragraph 36
                                                                                 ] )
                                                     ]
                            })) `shouldBe` "flowchart TD\n\
                                           \  classDef deadEnd fill:#CD5C5C,color:#ffffff,stroke:#000000\n\
                                           \  classDef unknown fill:#DCDCDC,stroke:#000000\n\
                                           \  classDef solution fill:#008000,color:#ffffff,stroke:#000000\n\
                                           \  n1[1]:::solution\n\
                                           \  n63[63]:::solution\n\
                                           \  n1 --> n63\n\
                                           \  n88(88):::unknown\n\
                                           \  n1 --> n88\n\
                                           \  n36[36]:::deadEnd\n\
                                           \  n36 --> n63\n\
                                           \  n17(17):::unknown\n\
                                           \  n63 --> n17\n\
                                           \  n100[100]:::solution\n\
                                           \  n63 --> n100\n\
                                           \  n88 --> n17\n\
                                           \  n88 --> n36\n"

    it "should turn a graph into a Mermaid diagram (3)" $
      (graph2mermaid (Graph { nodes_ = M.empty
                            , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 22
                                                                                , Paragraph 105
                                                                                ] )
                                                     , (Paragraph 22, S.fromList [ Paragraph 13
                                                                                 , Paragraph 48
                                                                                 ] )
                                                     ]
                            })) `shouldBe` "flowchart TD\n\
                                           \  classDef deadEnd fill:#CD5C5C,color:#ffffff,stroke:#000000\n\
                                           \  classDef unknown fill:#DCDCDC,stroke:#000000\n\
                                           \  classDef solution fill:#008000,color:#ffffff,stroke:#000000\n\
                                           \  n1(1):::unknown\n\
                                           \  n22(22):::unknown\n\
                                           \  n1 --> n22\n\
                                           \  n105(105):::unknown\n\
                                           \  n1 --> n105\n\
                                           \  n13(13):::unknown\n\
                                           \  n22 --> n13\n\
                                           \  n48(48):::unknown\n\
                                           \  n22 --> n48\n"

    it "should turn a graph into a Mermaid diagram, even if the graph makes no sense" $
      (graph2mermaid (Graph { nodes_ = M.fromList [ (Paragraph 7, DeadEnd)
                                                  , (Paragraph 35, Unknown)
                                                  , (Paragraph 51, Unknown)
                                                  , (Paragraph 99, Solution)
                                                  ]
                            , vertices_ = M.fromList [ (Paragraph 7, S.fromList [ Paragraph 35
                                                                                ] )
                                                     , (Paragraph 51, S.fromList [ Paragraph 99
                                                                                 ] )
                                                     ]
                            })) `shouldBe` "flowchart TD\n\
                                           \  classDef deadEnd fill:#CD5C5C,color:#ffffff,stroke:#000000\n\
                                           \  classDef unknown fill:#DCDCDC,stroke:#000000\n\
                                           \  classDef solution fill:#008000,color:#ffffff,stroke:#000000\n\
                                           \  n7[7]:::deadEnd\n\
                                           \  n35(35):::unknown\n\
                                           \  n7 --> n35\n\
                                           \  n51(51):::unknown\n\
                                           \  n99[99]:::solution\n\
                                           \  n51 --> n99\n"
