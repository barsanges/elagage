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

arrows :: M.Map Paragraph [Paragraph]
arrows = M.fromList [ ( Paragraph 1, [ Paragraph 3
                                     , Paragraph 4
                                     , Paragraph 8
                                     , Paragraph 9
                                     ] )
                    , ( Paragraph 2, [ Paragraph 10 ] )
                    , ( Paragraph 4, [ Paragraph 5
                                     , Paragraph 7
                                     ] )
                    , ( Paragraph 5, [ Paragraph 7 ] )
                    , ( Paragraph 6, [ Paragraph 5 ] )
                    , ( Paragraph 8, [ Paragraph 5
                                     , Paragraph 6
                                     ] )
                    , ( Paragraph 9, [ Paragraph 2
                                     , Paragraph 3
                                     ] )
                    , ( Paragraph 10, [ Paragraph 2 ] )
                    ]

deadEnds :: M.Map Paragraph Criteria
deadEnds = M.fromList [ ( Paragraph 5, Just $ Conditions { hasNone = S.empty
                                                         , hasAll = [ S.singleton $ Paragraph 6 ]
                                                         } )
                      , ( Paragraph 9, Nothing )
                      ]

solutions :: M.Map Paragraph Criteria
solutions = M.fromList [ ( Paragraph 7, Just $ Conditions { hasNone = S.empty
                                                          , hasAll = [ S.fromList [ Paragraph 8 ] ]
                                                          } )
                       ]


spec :: Spec
spec = do
  describe "raw2tree" $ do
    it "should turn raw data into a tree (1)" $
      (raw2tree (Paragraph 11) M.empty M.empty M.empty) `shouldBe` (Leaf (Paragraph 11) Solution)

    it "should turn raw data into a tree (2)" $
      (raw2tree (Paragraph 1) arrows M.empty M.empty) `shouldBe` ( Branch (Paragraph 1) Solution
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
      (raw2tree (Paragraph 1) arrows deadEnds M.empty) `shouldBe` ( Branch (Paragraph 1) Solution
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
      (raw2tree (Paragraph 1) arrows M.empty solutions) `shouldBe` ( Branch (Paragraph 1) Solution
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
                                                                     ] )

    it "should turn raw data into a tree (5)" $
      (raw2tree (Paragraph 1) arrows deadEnds solutions) `shouldBe` ( Branch (Paragraph 1) Solution
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
                                                                      ] )
