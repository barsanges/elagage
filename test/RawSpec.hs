{- |
   Module      : RawSpec
   Copyright   : Copyright (C) 2026 barsanges

Teste le module `Raw`.
-}

module RawSpec ( spec ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Aeson
import Test.Hspec
import Raw

spec :: Spec
spec = do
  describe "JSON parser" $ do
    it "should allow repeated paragraph keys in arrows" $ do
      res <- eitherDecodeFileStrict "test/data/arrows-set.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4 ]
                                                                     , deadEnd_ = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 12
                                                                                                                                , Paragraph 14
                                                                                                                                , Paragraph 5
                                                                                                                                ]
                                                                                                         , hasAll = [ S.fromList [ Paragraph 17
                                                                                                                                 , Paragraph 6
                                                                                                                                 , Paragraph 9
                                                                                                                                 ]
                                                                                                                    ]
                                                                                                         }
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 2, Component { arrows_ = S.fromList [ Paragraph 11
                                                                                            , Paragraph 6
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 5
                                                                                                                              , Paragraph 19
                                                                                                                              ]
                                                                                                       , hasAll = []
                                                                                                       }
                                                                     } )
                                          ] )

    it "should allow comments / unused fields" $ do
      res <- eitherDecodeFileStrict "test/data/comment.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4 ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          ] )

    it "should allow repeated paragraph keys in sets" $ do
      res <- eitherDecodeFileStrict "test/data/criteria-set.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4 ]
                                                                      , deadEnd_ = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 27 ]
                                                                                                          , hasAll = [ S.fromList [ Paragraph 18
                                                                                                                                  , Paragraph 9
                                                                                                                                  ]
                                                                                                                     , S.fromList [ Paragraph 3 ]
                                                                                                                     ]
                                                                                                          }
                                                                      , final_ = Sometimes $ Conditions { hasNone = S.empty
                                                                                                        , hasAll = [ S.fromList [ Paragraph 16
                                                                                                                                , Paragraph 17
                                                                                                                                ]
                                                                                                                   , S.fromList [ Paragraph 4
                                                                                                                                , Paragraph 8
                                                                                                                                ]
                                                                                                                   ]
                                                                                                        }
                                                                      } )
                                          ] )

    it "should allow disjoint graphs" $ do
      res <- eitherDecodeFileStrict "test/data/disjoint.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4 ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 5, Component { arrows_ = S.fromList [ Paragraph 11 ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 11, Component { arrows_ = S.fromList [ Paragraph 3 ]
                                                                      , deadEnd_ = Never
                                                                      , final_ = Never
                                                                      } )
                                          ] )

    it "should be able to parse well formed but empty files" $ do
      res <- eitherDecodeFileStrict "test/data/empty.json" :: (IO (Either String Raw))
      res `shouldBe` (Right $ M.empty)

    it "should allow components with no arrow" $ do
      res <- eitherDecodeFileStrict "test/data/no-arrow.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 2, Component { arrows_ = S.fromList []
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList []
                                                                     , deadEnd_ = Always
                                                                     , final_ = Never
                                                                     } )
                                          ] )

    it "should allow components with no dead end" $ do
      res <- eitherDecodeFileStrict "test/data/no-dead-end.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4 ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          ] )

    it "should allow components with no field at all" $ do
      res <- eitherDecodeFileStrict "test/data/no-field.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList []
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          ] )

    it "should allow components with no final" $ do
      res <- eitherDecodeFileStrict "test/data/no-final.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4 ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          ] )

    it "should allow the first paragraph to be missing" $ do
      res <- eitherDecodeFileStrict "test/data/no-start.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4
                                                                                            , Paragraph 11
                                                                                            , Paragraph 5
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 5, Component { arrows_ = S.fromList [ Paragraph 11 ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          ] )

    it "Aeson does not reject data with repeated keys" $ do
      res <- eitherDecodeFileStrict "test/data/repeated-key.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4 ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Always
                                                                     } )
                                          ] )

    it "should reject empty files" $ do
      res <- eitherDecodeFileStrict "test/data/void.json" :: (IO (Either String Raw))
      res `shouldBe` (Left "Unexpected end-of-input, expecting JSON value")

    it "should parse Criteria" $ do
      res <- eitherDecodeFileStrict "test/data/vanilla.json" :: (IO (Either String Raw))
      res `shouldBe` ( Right $ M.fromList [ ( Paragraph 1, Component { arrows_ = S.fromList [ Paragraph 2
                                                                                            , Paragraph 8
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 8, Component { arrows_ = S.fromList [ Paragraph 4 ]
                                                                     , deadEnd_ = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 12
                                                                                                                                , Paragraph 14
                                                                                                                                , Paragraph 5
                                                                                                                                ]
                                                                                                         , hasAll = [ S.fromList [ Paragraph 17
                                                                                                                                 , Paragraph 6
                                                                                                                                 , Paragraph 9
                                                                                                                                 ]
                                                                                                                    ]
                                                                                                         }
                                                                     , final_ = Never
                                                                     } )
                                          , ( Paragraph 2, Component { arrows_ = S.fromList [ Paragraph 11
                                                                                            , Paragraph 6
                                                                                            ]
                                                                     , deadEnd_ = Never
                                                                     , final_ = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 5
                                                                                                                              , Paragraph 19
                                                                                                                              ]
                                                                                                       , hasAll = []
                                                                                                       }
                                                                     } )
                                          , ( Paragraph 11, Component { arrows_ = S.fromList [ Paragraph 3 ]
                                                                      , deadEnd_ = Sometimes $ Conditions { hasNone = S.fromList [ Paragraph 27 ]
                                                                                                          , hasAll = [ S.fromList [ Paragraph 18
                                                                                                                                  , Paragraph 9
                                                                                                                                  ]
                                                                                                                     , S.fromList [ Paragraph 3 ]
                                                                                                                     ]
                                                                                                          }
                                                                      , final_ = Sometimes $ Conditions { hasNone = S.empty
                                                                                                        , hasAll = [ S.fromList [ Paragraph 16
                                                                                                                                , Paragraph 17
                                                                                                                                ]
                                                                                                                   , S.fromList [ Paragraph 4
                                                                                                                                , Paragraph 8
                                                                                                                                ]
                                                                                                                   ]
                                                                                                        }
                                                                      } )
                                          ] )

    it "should reject data where arrows have the wrong type" $ do
      res <- eitherDecodeFileStrict "test/data/wrong-type-arrows.json" :: (IO (Either String Raw))
      res `shouldBe` (Left "Error in $['8'].correspondances: parsing [] failed, expected Array, but encountered String")

    it "should reject data where dead ends have the wrong type" $ do
      res <- eitherDecodeFileStrict "test/data/wrong-type-dead-ends.json" :: (IO (Either String Raw))
      res `shouldBe` (Left "Error in $['8']['cul de sac']: parsing Paragraph failed, expected Bool | Object, but encountered Number")

    it "should reject data where finals have the wrong type" $ do
      res <- eitherDecodeFileStrict "test/data/wrong-type-finals.json" :: (IO (Either String Raw))
      res `shouldBe` (Left "Error in $['8'].fin: parsing Paragraph failed, expected Bool | Object, but encountered Number")

    it "should reject data where keys have the wrong type" $ do
      res <- eitherDecodeFileStrict "test/data/wrong-type-key.json" :: (IO (Either String Raw))
      res `shouldBe` (Left "Unexpected \"1:\\n    {\\n\\t\\\"correspondances\\\": [\", expecting record key literal or }")

    it "should reject data which is not a JSON object" $ do
      res <- eitherDecodeFileStrict "test/data/wrong-type-raw.json" :: (IO (Either String Raw))
      res `shouldBe` (Left "Error in $: parsing Map failed, expected Object, but encountered Array")
