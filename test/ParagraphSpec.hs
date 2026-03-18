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
cond1 = Just $ Conditions { hasNone = S.fromList [ Paragraph 2
                                                 , Paragraph 4
                                                 ]
                          , hasAll = []
                          }


cond2 :: Criteria
cond2 = Just $ Conditions { hasNone = S.empty
                          , hasAll = [ S.fromList [ Paragraph 3
                                                  , Paragraph 5
                                                  ]
                                     ]
                          }

cond3 :: Criteria
cond3 = Just $ Conditions { hasNone = S.fromList [ Paragraph 2
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
cond4 = Just $ Conditions { hasNone = S.fromList [ Paragraph 2
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
    it "should always return False if there is no Criteria" $
      (eval Nothing []) `shouldBe` False

    it "should always return False if there is no Criteria" $
      (eval Nothing [Paragraph 1, Paragraph 8]) `shouldBe` False

    it "should always return True if there is no Conditions" $
      (eval (Just Nothing) []) `shouldBe` True

    it "should always return True if there is no Conditions" $
      (eval (Just Nothing) [Paragraph 1, Paragraph 8]) `shouldBe` True

    it "should return True if the path does not contain any excluded nodes" $
      (eval (Just cond1) [Paragraph 1, Paragraph 5]) `shouldBe` True

    it "should return False if the path contains any excluded nodes (1)" $
      (eval (Just cond1) [Paragraph 4, Paragraph 5]) `shouldBe` False

    it "should return False if the path contains any excluded nodes (2)" $
      (eval (Just cond1) [Paragraph 4, Paragraph 2]) `shouldBe` False

    it "should return True if the path contains at least one node in each required category (1)" $
      (eval (Just cond2) [Paragraph 5]) `shouldBe` True

    it "should return True if the path contains at least one node in each required category (2)" $
      (eval (Just cond2) [Paragraph 5, Paragraph 3]) `shouldBe` True

    it "should return False if the path does not contain at least one node in each required category (1)" $
      (eval (Just cond2) [Paragraph 2, Paragraph 4]) `shouldBe` False

    it "should return False if the path does not contain at least one node in each required category (2)" $
      (eval (Just cond3) [Paragraph 3, Paragraph 5]) `shouldBe` False

    it "should return True if the path does not contain any excluded nodes and contains at least one node in each required category (1)" $
      (eval (Just cond3) [Paragraph 5, Paragraph 9]) `shouldBe` True

    it "should return True if the path does not contain any excluded nodes and contains at least one node in each required category (2)" $
      (eval (Just cond4) [Paragraph 3, Paragraph 9]) `shouldBe` True

    it "should return False if the path contains any excluded nodes, even if it is also a required one" $
      (eval (Just cond4) [Paragraph 4, Paragraph 9]) `shouldBe` False

    it "should return False if the path contains any excluded nodes, even if it contains at least one node in each required category" $
      (eval (Just cond3) [Paragraph 5, Paragraph 9, Paragraph 4]) `shouldBe` False

    it "should return False if the path does not contain at least one node in each required category, even if it does not contain any excluded nodes" $
      (eval (Just cond3) [Paragraph 3]) `shouldBe` False
