{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Raw
   Copyright   : Copyright (C) 2026 barsanges

Les données connues à date pour un livre.
-}

module Raw
  ( module Paragraph
  , Raw
  , Component(..)
  ) where

import Data.Aeson
import Data.Aeson.Types ( Parser, prependFailure, typeMismatch )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import Text.Read ( readEither )

import Paragraph

-- | Les données connues à date pour un livre.
type Raw = M.Map Paragraph Component

-- | Les données connues à date pour un paragraphe du livre.
data Component = Component { arrows_ :: [Paragraph]
                           , deadEnd_ :: Criteria
                           , final_ :: Criteria
                           }
  deriving (Show, Eq)

-- FIXME : pour une prochaine utilisation d'Aeson, cela vaudrait le coup
-- d'essayer de parser un `Value` uniquement et de dépiauter ça à la main
-- plutôt que dans Aeson.
instance FromJSON Paragraph where
  parseJSON = withText "Paragraph" go
    where
      go t = case (readEither $ T.unpack t) of
        Left m -> fail m
        Right x -> return (Paragraph x)

instance FromJSONKey Paragraph where
  fromJSONKey = FromJSONKeyTextParser go
    where
      go t = case (readEither $ T.unpack t) of
        Left m -> fail m
        Right x -> return (Paragraph x)

instance FromJSON Conditions where
  parseJSON = withObject "Conditions" go
    where
      go v = do
        none_ <- v .: "noeuds inhibiteurs"
        all_ <- v .: "noeuds déclencheurs"
        return (Conditions { hasNone = none_
                           , hasAll = all_
                           })

instance FromJSON Criteria where
  parseJSON (Bool True) = return Always
  parseJSON (Bool False) = return Never
  parseJSON (Object v) = do
    x <- parseJSON (Object v)
    return (Sometimes x)
  parseJSON (Array x) = prependContext "Paragraph" (typeMismatch "Bool | Object" $ Array x)
  parseJSON (String x) = prependContext "Paragraph" (typeMismatch "Bool | Object" $ String x)
  parseJSON (Number x) = prependContext "Paragraph" (typeMismatch "Bool | Object" $ Number x)
  parseJSON Null = prependContext "Paragraph" (typeMismatch "Bool | Object" Null)

instance FromJSON Component where
  parseJSON = withObject "Component" go
    where
      go v = do
        arrows <- fmap (fromMaybe []) $ v .:? "correspondances"
        deadEnd <- fmap (fromMaybe Never) $ v .:? "cul de sac"
        final <- fmap (fromMaybe Never) $ v .:? "fin"
        return Component { arrows_ = arrows
                         , deadEnd_ = deadEnd
                         , final_ = final
                         }

-- | Ajoute le contexte à un message d'erreur Aeson (la fonction
-- existe telle quelle dans la librairie mais n'est pas exportée).
prependContext :: String -> Parser a -> Parser a
prependContext name = prependFailure ("parsing " ++ name ++ " failed, ")
