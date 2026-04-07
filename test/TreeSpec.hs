{- |
   Module      : TreeSpec
   Copyright   : Copyright (C) 2026 barsanges

Teste le module `Tree`.
-}

module TreeSpec ( spec ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Test.Hspec
import Tree

spec :: Spec
spec = do
  describe "raw2tree" $ do
    it "should turn raw data into a tree (1)" $
      (raw2tree (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList []
                                                       , deadEnd_ = Never
                                                       , final_ = Always
                                                       }
                              )
                            ] ) ) `shouldBe` (Leaf (Paragraph 1) Solution)

    it "should turn raw data into a tree (2)" $
      (raw2tree (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 3
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
                            ] ) ) `shouldBe` ( Branch (Paragraph 1) Solution
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
                                               ] )

    it "should turn raw data into a tree (3)" $
      (raw2tree (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 3
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
                            ] ) ) `shouldBe` ( Branch (Paragraph 1) Solution
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
                                               ] )

    it "should turn raw data into a tree (4)" $
      (raw2tree (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 3
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
                            ] ) ) `shouldBe` ( Branch (Paragraph 1) Solution
                                               [ Leaf (Paragraph 3) Unknown
                                               , Branch (Paragraph 4) DeadEnd
                                                 [ Branch (Paragraph 5) DeadEnd
                                                   [ Leaf (Paragraph 7) DeadEnd ]
                                                 , Leaf (Paragraph 7) DeadEnd
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
                                               ] )

    it "should turn raw data into a tree (5)" $
      (raw2tree (M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 3
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
                            ] ) ) `shouldBe` ( Branch (Paragraph 1) Solution
                                               [ Leaf (Paragraph 3) Unknown
                                               , Branch (Paragraph 4) DeadEnd
                                                 [ Branch (Paragraph 5) DeadEnd
                                                   [ Leaf (Paragraph 7) DeadEnd ]
                                                 , Leaf (Paragraph 7) DeadEnd
                                                 ]
                                               , Branch (Paragraph 8) Solution
                                                 [ Branch (Paragraph 5) Solution
                                                   [ Leaf (Paragraph 7) Solution ]
                                                 , Branch (Paragraph 6) DeadEnd
                                                   [ Leaf (Paragraph 5) DeadEnd ]
                                                 ]
                                               , Leaf (Paragraph 9) DeadEnd
                                               ] )

    it "should turn raw data into a tree (6)" $
      (raw2tree (M.fromList [ ( Paragraph 7, Component { arrows_ = S.fromList []
                                                       , deadEnd_ = Never
                                                       , final_ = Always
                                                       }
                              )
                            ] ) ) `shouldBe` (Leaf (Paragraph 1) Solution)
