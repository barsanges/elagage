{- |
   Module      : GraphSpec
   Copyright   : Copyright (C) 2026 barsanges

Teste le module `Graph`.
-}

module GraphSpec ( spec ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.Hspec
import Graph

tree1 :: Tree
tree1 = Leaf (Paragraph 11) Solution

tree2 :: Tree
tree2 = Branch (Paragraph 1) Solution
        [ Leaf (Paragraph 3) Unknown
        , Branch (Paragraph 4) Unknown
          [ Branch (Paragraph 5) Unknown
            [ Leaf (Paragraph 7) Unknown ]
          , Leaf (Paragraph 7) Unknown
          ]
        , Branch (Paragraph 8) Unknown
          [ Branch (Paragraph 5) Unknown
            [ Leaf (Paragraph 7) Unknown ]
          , Branch (Paragraph 6) Unknown
            [ Branch (Paragraph 5) Unknown
              [ Leaf (Paragraph 7) Unknown ]
            ]
          ]
        , Branch (Paragraph 9) Unknown
          [ Branch (Paragraph 2) DeadEnd
            [ Branch (Paragraph 10) DeadEnd
              [ Leaf (Paragraph 2) DeadEnd ]
            ]
          , Leaf (Paragraph 3) Unknown
          ]
        ]

tree3 :: Tree
tree3 = Branch (Paragraph 1) Solution
        [ Leaf (Paragraph 3) Unknown
        , Branch (Paragraph 4) Unknown
          [ Branch (Paragraph 5) Unknown
            [ Leaf (Paragraph 7) Unknown ]
          , Leaf (Paragraph 7) Unknown
          ]
        , Branch (Paragraph 8) Unknown
          [ Branch (Paragraph 5) Unknown
            [ Leaf (Paragraph 7) Unknown ]
          , Branch (Paragraph 6) DeadEnd
            [ Leaf (Paragraph 5) DeadEnd ]
          ]
        , Leaf (Paragraph 9) DeadEnd
        ]

tree4 :: Tree
tree4 = Branch (Paragraph 1) Solution
        [ Leaf (Paragraph 3) Unknown
        , Branch (Paragraph 4) Unknown
          [ Branch (Paragraph 5) Unknown
            [ Leaf (Paragraph 7) Unknown ]
          , Leaf (Paragraph 7) Unknown
          ]
        , Branch (Paragraph 8) Solution
          [ Branch (Paragraph 5) Solution
            [ Leaf (Paragraph 7) Solution ]
          , Branch (Paragraph 6) Solution
            [ Branch (Paragraph 5) Solution
              [ Leaf (Paragraph 7) Solution ]
            ]
          ]
        , Branch (Paragraph 9) Unknown
          [ Branch (Paragraph 2) DeadEnd
            [ Branch (Paragraph 10) DeadEnd
              [ Leaf (Paragraph 2) DeadEnd ]
            ]
          , Leaf (Paragraph 3) Unknown
          ]
        ]

tree5 :: Tree
tree5 = Branch (Paragraph 1) Solution
        [ Leaf (Paragraph 3) Unknown
        , Branch (Paragraph 4) Unknown
          [ Branch (Paragraph 5) Unknown
            [ Leaf (Paragraph 7) Unknown ]
          , Leaf (Paragraph 7) Unknown
          ]
        , Branch (Paragraph 8) Solution
          [ Branch (Paragraph 5) Solution
            [ Leaf (Paragraph 7) Solution ]
          , Branch (Paragraph 6) DeadEnd
            [ Leaf (Paragraph 5) DeadEnd ]
          ]
        , Leaf (Paragraph 9) DeadEnd
        ]

spec :: Spec
spec = do
  describe "raw2graph" $ do
    it "should turn raw data into a graph (1)" $
      (raw2graph (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList []
                                                        , deadEnd_ = Never
                                                        , final_ = Always
                                                        }
                              )
                            ] ) ) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution) ]
                                                   , vertices_ = M.empty
                                                   }

    it "should turn raw data into a graph (2)" $
      (raw2graph (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 3
                                                                               , Paragraph 4
                                                                               , Paragraph 8
                                                                               , Paragraph 9
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 2, Component { arrows_ = S.fromList [ Paragraph 10 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 4, Component { arrows_ = S.fromList [ Paragraph 5
                                                                              , Paragraph 7
                                                                              ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 5, Component { arrows_ = S.fromList [ Paragraph 7 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 6, Component { arrows_ = S.fromList [ Paragraph 5 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                       } )
                             , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 5
                                                                               , Paragraph 6
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 9, Component { arrows_ = S.fromList [ Paragraph 2
                                                                               , Paragraph 3
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                            , ( Paragraph 10, Component { arrows_ = S.fromList [ Paragraph 2 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                            ] ) ) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                         , (Paragraph 2, DeadEnd)
                                                                         , (Paragraph 3, Unknown)
                                                                         , (Paragraph 4, Unknown)
                                                                         , (Paragraph 5, Unknown)
                                                                         , (Paragraph 6, Unknown)
                                                                         , (Paragraph 7, Unknown)
                                                                         , (Paragraph 8, Unknown)
                                                                         , (Paragraph 9, Unknown)
                                                                         , (Paragraph 10, DeadEnd)
                                                                         ]
                                                   , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 3
                                                                                                       , Paragraph 4
                                                                                                       , Paragraph 8
                                                                                                       , Paragraph 9
                                                                                                       ] )
                                                                            , (Paragraph 2, S.fromList [ Paragraph 10 ] )
                                                                            , (Paragraph 4, S.fromList [ Paragraph 5
                                                                                                       , Paragraph 7
                                                                                                       ] )
                                                                            , (Paragraph 5, S.fromList [ Paragraph 7 ] )
                                                                            , (Paragraph 6, S.fromList [ Paragraph 5 ] )
                                                                            , (Paragraph 8, S.fromList [ Paragraph 5
                                                                                                       , Paragraph 6
                                                                                                       ] )
                                                                            , (Paragraph 9, S.fromList [ Paragraph 2
                                                                                                       , Paragraph 3
                                                                                                       ] )
                                                                            , (Paragraph 10, S.fromList [ Paragraph 2] )
                                                                            ]
                                                   }

    it "should turn raw data into a graph (3)" $
      (raw2graph (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 3
                                                                               , Paragraph 4
                                                                               , Paragraph 8
                                                                               , Paragraph 9
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 2, Component { arrows_ = S.fromList [ Paragraph 10 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                       } )
                             , ( Paragraph 4, Component { arrows_ = S.fromList [ Paragraph 5
                                                                               , Paragraph 7
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 5, Component { arrows_ = S.fromList [ Paragraph 7 ]
                                                        , deadEnd_ = Sometimes $ Conditions { hasNone = S.empty
                                                                                            , hasAll = [ S.singleton $ Paragraph 6 ]
                                                                                            }
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 6, Component { arrows_ = S.fromList [ Paragraph 5 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 5
                                                                               , Paragraph 6
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                            , ( Paragraph 9, Component { arrows_ = S.fromList [ Paragraph 2
                                                                              , Paragraph 3
                                                                              ]
                                                       , deadEnd_ = Always
                                                       , final_ = Never
                                                       } )
                            , ( Paragraph 10, Component { arrows_ = S.fromList [ Paragraph 2 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                            ] ) ) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                         , (Paragraph 3, Unknown)
                                                                         , (Paragraph 4, Unknown)
                                                                         , (Paragraph 5, Unknown)
                                                                         , (Paragraph 6, DeadEnd)
                                                                         , (Paragraph 7, Unknown)
                                                                         , (Paragraph 8, Unknown)
                                                                         , (Paragraph 9, DeadEnd)
                                                                         ]
                                                   , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 3
                                                                                                       , Paragraph 4
                                                                                                       , Paragraph 8
                                                                                                       , Paragraph 9
                                                                                                       ] )
                                                                            , (Paragraph 4, S.fromList [ Paragraph 5
                                                                                                       , Paragraph 7
                                                                                                       ] )
                                                                            , (Paragraph 5, S.fromList [ Paragraph 7 ] )
                                                                            , (Paragraph 6, S.fromList [ Paragraph 5 ] )
                                                                            , (Paragraph 8, S.fromList [ Paragraph 5
                                                                                                       , Paragraph 6
                                                                                                       ] )
                                                                            ]
                                                   }

    it "should turn raw data into a graph (4)" $
      (raw2graph (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 3
                                                                               , Paragraph 4
                                                                               , Paragraph 8
                                                                               , Paragraph 9
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 2, Component { arrows_ = S.fromList [ Paragraph 10 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 4, Component { arrows_ = S.fromList [ Paragraph 5
                                                                               , Paragraph 7
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 5, Component { arrows_ = S.fromList [ Paragraph 7 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 6, Component { arrows_ = S.fromList [ Paragraph 5 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 7, Component { arrows_ = S.fromList []
                                                        , deadEnd_ = Never
                                                        , final_ = Sometimes $ Conditions { hasNone = S.empty
                                                                                          , hasAll = [ S.fromList [ Paragraph 8 ] ]
                                                                                          }
                                                        } )
                             , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 5
                                                                               , Paragraph 6
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 9, Component { arrows_ = S.fromList [ Paragraph 2
                                                                               , Paragraph 3
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 10, Component { arrows_ = S.fromList [ Paragraph 2 ]
                                                         , deadEnd_ = Never
                                                         , final_ = Never
                                                         } )
                             ] ) ) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                          , (Paragraph 2, DeadEnd)
                                                                          , (Paragraph 3, Unknown)
                                                                          , (Paragraph 4, DeadEnd)
                                                                          , (Paragraph 5, Solution)
                                                                          , (Paragraph 6, Solution)
                                                                          , (Paragraph 7, Solution)
                                                                          , (Paragraph 8, Solution)
                                                                          , (Paragraph 9, Unknown)
                                                                          , (Paragraph 10, DeadEnd)
                                                                          ]
                                                    , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 3
                                                                                                        , Paragraph 4
                                                                                                        , Paragraph 8
                                                                                                        , Paragraph 9
                                                                                                        ] )
                                                                             , (Paragraph 2, S.fromList [ Paragraph 10 ] )
                                                                             , (Paragraph 4, S.fromList [ Paragraph 5
                                                                                                        , Paragraph 7
                                                                                                        ] )
                                                                             , (Paragraph 5, S.fromList [ Paragraph 7 ] )
                                                                             , (Paragraph 6, S.fromList [ Paragraph 5 ] )
                                                                             , (Paragraph 8, S.fromList [ Paragraph 5
                                                                                                        , Paragraph 6
                                                                                                        ] )
                                                                             , (Paragraph 9, S.fromList [ Paragraph 2
                                                                                                        , Paragraph 3
                                                                                                        ] )
                                                                             , (Paragraph 10, S.fromList [ Paragraph 2] )
                                                                             ]
                                                    }

    it "should turn raw data into a graph (5)" $
      (raw2graph (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 3
                                                                               , Paragraph 4
                                                                               , Paragraph 8
                                                                               , Paragraph 9
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 2, Component { arrows_ = S.fromList [ Paragraph 10 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 4, Component { arrows_ = S.fromList [ Paragraph 5
                                                                               , Paragraph 7
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 5, Component { arrows_ = S.fromList [ Paragraph 7 ]
                                                        , deadEnd_ = Sometimes $ Conditions { hasNone = S.empty
                                                                                            , hasAll = [ S.singleton $ Paragraph 6 ]
                                                                                            }
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 6, Component { arrows_ = S.fromList [ Paragraph 5 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 7, Component { arrows_ = S.fromList []
                                                        , deadEnd_ = Never
                                                        , final_ = Sometimes $ Conditions { hasNone = S.empty
                                                                                          , hasAll = [ S.fromList [ Paragraph 8 ] ]
                                                                                          }
                                                        } )
                             , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 5
                                                                               , Paragraph 6
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 9, Component { arrows_ = S.fromList [ Paragraph 2
                                                                               , Paragraph 3
                                                                               ]
                                                        , deadEnd_ = Always
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 10, Component { arrows_ = S.fromList [ Paragraph 2 ]
                                                         , deadEnd_ = Never
                                                         , final_ = Never
                                                         } )
                             ] ) ) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                          , (Paragraph 3, Unknown)
                                                                          , (Paragraph 4, DeadEnd)
                                                                          , (Paragraph 5, Solution)
                                                                          , (Paragraph 6, DeadEnd)
                                                                          , (Paragraph 7, Solution)
                                                                          , (Paragraph 8, Solution)
                                                                          , (Paragraph 9, DeadEnd)
                                                                          ]
                                                    , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 3
                                                                                                        , Paragraph 4
                                                                                                        , Paragraph 8
                                                                                                        , Paragraph 9
                                                                                                        ] )
                                                                             , (Paragraph 4, S.fromList [ Paragraph 5
                                                                                                        , Paragraph 7
                                                                                                        ] )
                                                                             , (Paragraph 5, S.fromList [ Paragraph 7 ] )
                                                                             , (Paragraph 6, S.fromList [ Paragraph 5 ] )
                                                                             , (Paragraph 8, S.fromList [ Paragraph 5
                                                                                                        , Paragraph 6
                                                                                                        ] )
                                                                             ]
                                                    }

    it "should create a graph that always starts with the paragraph '1'" $
      (raw2graph (M.fromList [ ( Paragraph 7, Component { arrows_ = S.fromList []
                                                        , deadEnd_ = Always
                                                        , final_ = Never
                                                        }
                               )
                             ] ) ) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution) ]
                                                    , vertices_ = M.empty
                                                    }

    it "should handle cycles gracefully (1)" $
      (raw2graph (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                            , ( Paragraph 2, Component { arrows_ = S.fromList [ Paragraph 3 ]
                                                       , deadEnd_ = Never
                                                       , final_ = Never
                                                       } )
                            , ( Paragraph 3, Component { arrows_ = S.fromList [ Paragraph 2 ]
                                                       , deadEnd_ = Never
                                                       , final_ = Never
                                                       } )
                            ] ) ) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                         , (Paragraph 2, DeadEnd)
                                                                         , (Paragraph 3, DeadEnd)
                                                                         ]
                                                   , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 2 ] )
                                                                            , (Paragraph 2, S.fromList [ Paragraph 3 ] )
                                                                            , (Paragraph 3, S.fromList [ Paragraph 2 ] )
                                                                            ]
                                                   }

    it "should handle cycles gracefully (2)" $
      (raw2graph (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                               , Paragraph 6
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 2, Component { arrows_ = S.fromList [ Paragraph 3 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 3, Component { arrows_ = S.fromList [ Paragraph 4
                                                                               , Paragraph 5
                                                                               ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 4, Component { arrows_ = S.fromList [ Paragraph 5 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                             , ( Paragraph 5, Component { arrows_ = S.fromList [ Paragraph 2 ]
                                                        , deadEnd_ = Never
                                                        , final_ = Never
                                                        } )
                            ] ) ) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                         , (Paragraph 2, DeadEnd)
                                                                         , (Paragraph 3, DeadEnd)
                                                                         , (Paragraph 4, DeadEnd)
                                                                         , (Paragraph 5, DeadEnd)
                                                                         , (Paragraph 6, Unknown)
                                                                         ]
                                                   , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 2
                                                                                                       , Paragraph 6
                                                                                                       ] )
                                                                            , (Paragraph 2, S.fromList [ Paragraph 3 ] )
                                                                            , (Paragraph 3, S.fromList [ Paragraph 4
                                                                                                       , Paragraph 5
                                                                                                       ] )
                                                                            , (Paragraph 4, S.fromList [ Paragraph 5 ] )
                                                                            , (Paragraph 5, S.fromList [ Paragraph 2 ] )
                                                                            ]
                                                   }

  describe "tree2graph" $ do
    it "should turn a tree into a graph (1)" $
      (tree2graph tree1) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 11, Solution) ]
                                          , vertices_ = M.empty
                                          }

    it "should turn a tree into a graph (2)" $
      (tree2graph tree2) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                , (Paragraph 2, DeadEnd)
                                                                , (Paragraph 3, Unknown)
                                                                , (Paragraph 4, Unknown)
                                                                , (Paragraph 5, Unknown)
                                                                , (Paragraph 6, Unknown)
                                                                , (Paragraph 7, Unknown)
                                                                , (Paragraph 8, Unknown)
                                                                , (Paragraph 9, Unknown)
                                                                , (Paragraph 10, DeadEnd)
                                                                ]
                                          , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 3
                                                                                              , Paragraph 4
                                                                                              , Paragraph 8
                                                                                              , Paragraph 9
                                                                                              ] )
                                                                   , (Paragraph 2, S.fromList [ Paragraph 10 ] )
                                                                   , (Paragraph 4, S.fromList [ Paragraph 5
                                                                                              , Paragraph 7
                                                                                              ] )
                                                                   , (Paragraph 5, S.fromList [ Paragraph 7 ] )
                                                                   , (Paragraph 6, S.fromList [ Paragraph 5 ] )
                                                                   , (Paragraph 8, S.fromList [ Paragraph 5
                                                                                              , Paragraph 6
                                                                                              ] )
                                                                   , (Paragraph 9, S.fromList [ Paragraph 2
                                                                                              , Paragraph 3
                                                                                              ] )
                                                                   , (Paragraph 10, S.fromList [ Paragraph 2] )
                                                                   ]
                                          }

    it "should turn a tree into a graph (3)" $
      (tree2graph tree3) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                , (Paragraph 3, Unknown)
                                                                , (Paragraph 4, Unknown)
                                                                , (Paragraph 5, Unknown)
                                                                , (Paragraph 6, DeadEnd)
                                                                , (Paragraph 7, Unknown)
                                                                , (Paragraph 8, Unknown)
                                                                , (Paragraph 9, DeadEnd)
                                                                ]
                                          , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 3
                                                                                              , Paragraph 4
                                                                                              , Paragraph 8
                                                                                              , Paragraph 9
                                                                                              ] )
                                                                   , (Paragraph 4, S.fromList [ Paragraph 5
                                                                                              , Paragraph 7
                                                                                              ] )
                                                                   , (Paragraph 5, S.fromList [ Paragraph 7 ] )
                                                                   , (Paragraph 6, S.fromList [ Paragraph 5 ] )
                                                                   , (Paragraph 8, S.fromList [ Paragraph 5
                                                                                              , Paragraph 6
                                                                                              ] )
                                                                   ]
                                          }

    it "should turn a tree into a graph (4)" $
      (tree2graph tree4) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                , (Paragraph 2, DeadEnd)
                                                                , (Paragraph 3, Unknown)
                                                                , (Paragraph 4, Unknown)
                                                                , (Paragraph 5, Solution)
                                                                , (Paragraph 6, Solution)
                                                                , (Paragraph 7, Solution)
                                                                , (Paragraph 8, Solution)
                                                                , (Paragraph 9, Unknown)
                                                                , (Paragraph 10, DeadEnd)
                                                                ]
                                          , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 3
                                                                                              , Paragraph 4
                                                                                              , Paragraph 8
                                                                                              , Paragraph 9
                                                                                              ] )
                                                                   , (Paragraph 2, S.fromList [ Paragraph 10 ] )
                                                                   , (Paragraph 4, S.fromList [ Paragraph 5
                                                                                              , Paragraph 7
                                                                                              ] )
                                                                   , (Paragraph 5, S.fromList [ Paragraph 7 ] )
                                                                   , (Paragraph 6, S.fromList [ Paragraph 5 ] )
                                                                   , (Paragraph 8, S.fromList [ Paragraph 5
                                                                                              , Paragraph 6
                                                                                              ] )
                                                                   , (Paragraph 9, S.fromList [ Paragraph 2
                                                                                              , Paragraph 3
                                                                                              ] )
                                                                   , (Paragraph 10, S.fromList [ Paragraph 2] )
                                                                   ]
                                          }

    it "should turn a tree into a graph (5)" $
      (tree2graph tree5) `shouldBe` Graph { nodes_ = M.fromList [ (Paragraph 1, Solution)
                                                                , (Paragraph 3, Unknown)
                                                                , (Paragraph 4, Unknown)
                                                                , (Paragraph 5, Solution)
                                                                , (Paragraph 6, DeadEnd)
                                                                , (Paragraph 7, Solution)
                                                                , (Paragraph 8, Solution)
                                                                , (Paragraph 9, DeadEnd)
                                                                ]
                                          , vertices_ = M.fromList [ (Paragraph 1, S.fromList [ Paragraph 3
                                                                                              , Paragraph 4
                                                                                              , Paragraph 8
                                                                                              , Paragraph 9
                                                                                              ] )
                                                                   , (Paragraph 4, S.fromList [ Paragraph 5
                                                                                              , Paragraph 7
                                                                                              ] )
                                                                   , (Paragraph 5, S.fromList [ Paragraph 7 ] )
                                                                   , (Paragraph 6, S.fromList [ Paragraph 5 ] )
                                                                   , (Paragraph 8, S.fromList [ Paragraph 5
                                                                                              , Paragraph 6
                                                                                              ] )
                                                                   ]
                                          }
