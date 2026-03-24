{- |
   Module      : ParagraphSpec
   Copyright   : Copyright (C) 2026 barsanges

Teste le module `Paragraph`.
-}

module ParagraphSpec ( spec ) where

import qualified Data.Set as S

import Test.Hspec
import Paragraph

cond1 :: Criteria
cond1 = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 2
                                                      , Paragraph 4
                                                      ]
                               , hasAll = []
                               }

cond2 :: Criteria
cond2 = Sometimes $ Conditions { hasNone = S.empty
                               , hasAll = [ S.fromList [ Paragraph 3
                                                       , Paragraph 5
                                                       ]
                                          ]
                               }

cond3 :: Criteria
cond3 = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 2
                                                      , Paragraph 4
                                                      , Paragraph 6
                                                      ]
                               , hasAll = [ S.fromList [ Paragraph 3
                                                       , Paragraph 5
                                                       ]
                                     , S.fromList [ Paragraph 9 ]
                                     ]
                               }

cond4 :: Criteria
cond4 = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 2
                                                      , Paragraph 4
                                                      ]
                               , hasAll = [ S.fromList [ Paragraph 3
                                                       , Paragraph 4
                                                       ]
                                          , S.fromList [ Paragraph 9 ]
                                          ]
                               }

spec :: Spec
spec = do
  describe "eval" $ do
    it "should always return False if the Criteria is Never" $
      (eval Never []) `shouldBe` False

    it "should always return False if the Criteria is Never" $
      (eval Never [Paragraph 1, Paragraph 8]) `shouldBe` False

    it "should always return True if the Criteria is Always" $
      (eval Always []) `shouldBe` True

    it "should always return True if the Criteria is Always" $
      (eval Always [Paragraph 1, Paragraph 8]) `shouldBe` True

    it "should return True if the path does not contain any excluded nodes" $
      (eval cond1 [Paragraph 1, Paragraph 5]) `shouldBe` True

    it "should return False if the path contains any excluded nodes (1)" $
      (eval cond1 [Paragraph 4, Paragraph 5]) `shouldBe` False

    it "should return False if the path contains any excluded nodes (2)" $
      (eval cond1 [Paragraph 4, Paragraph 2]) `shouldBe` False

    it "should return True if the path contains at least one node in each required category (1)" $
      (eval cond2 [Paragraph 5]) `shouldBe` True

    it "should return True if the path contains at least one node in each required category (2)" $
      (eval cond2 [Paragraph 5, Paragraph 3]) `shouldBe` True

    it "should return False if the path does not contain at least one node in each required category (1)" $
      (eval cond2 [Paragraph 2, Paragraph 4]) `shouldBe` False

    it "should return False if the path does not contain at least one node in each required category (2)" $
      (eval cond3 [Paragraph 3, Paragraph 5]) `shouldBe` False

    it "should return True if the path does not contain any excluded nodes and contains at least one node in each required category (1)" $
      (eval cond3 [Paragraph 5, Paragraph 9]) `shouldBe` True

    it "should return True if the path does not contain any excluded nodes and contains at least one node in each required category (2)" $
      (eval cond4 [Paragraph 3, Paragraph 9]) `shouldBe` True

    it "should return False if the path contains any excluded nodes, even if it is also a required one" $
      (eval cond4 [Paragraph 4, Paragraph 9]) `shouldBe` False

    it "should return False if the path contains any excluded nodes, even if it contains at least one node in each required category" $
      (eval cond3 [Paragraph 5, Paragraph 9, Paragraph 4]) `shouldBe` False

    it "should return False if the path does not contain at least one node in each required category, even if it does not contain any excluded nodes" $
      (eval cond3 [Paragraph 3]) `shouldBe` False
