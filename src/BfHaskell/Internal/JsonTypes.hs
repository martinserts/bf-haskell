-----------------------------------------------------------------------------
-- |
-- Module      :  BfHaskell.Internal.JsonTypes
-- Copyright   :  (C) 2020 Martins Erts
-- License     :  MIT
-- Maintainer  :  Martins Erts <martins.erts@gmail.com>
-- Stability   :  experimental

{-# LANGUAGE FlexibleContexts #-}

module BfHaskell.Internal.JsonTypes
(
    modifyJsonField
  , normalizeJsonFieldName
  , defaultFromJsonOptions
  , defaultToJsonOptions
  , defaultEnumToJsonOptions
  , defaultEnumFromJsonOptions
  , toJsonText
) where

import qualified Data.Aeson      as A
import qualified Data.Aeson.Text as A
import           Data.Char       (isUpper, toLower)
import           Data.Text       (Text)
import           Data.Text.Lazy  (toStrict)

lowerInitialChar :: String -> String
lowerInitialChar []     = []
lowerInitialChar (x:xs) = toLower x : xs

modifyJsonField :: Int -> String -> String
modifyJsonField c = lowerInitialChar . drop c

normalizeJsonFieldName :: String -> String
normalizeJsonFieldName = lowerInitialChar . dropWhile (not . isUpper)

normalizeJsonEnumFieldName :: String -> String
normalizeJsonEnumFieldName = tail . dropWhile (/= '_')

defaultFromJsonOptions :: A.Options
defaultFromJsonOptions = A.defaultOptions
                            { A.fieldLabelModifier = normalizeJsonFieldName }

defaultToJsonOptions :: A.Options
defaultToJsonOptions = A.defaultOptions
                        { A.fieldLabelModifier = normalizeJsonFieldName
                        , A.omitNothingFields = True }

defaultEnumToJsonOptions :: A.Options
defaultEnumToJsonOptions = A.defaultOptions
                            { A.constructorTagModifier = normalizeJsonEnumFieldName
                            , A.omitNothingFields = True }

defaultEnumFromJsonOptions :: A.Options
defaultEnumFromJsonOptions =
    A.defaultOptions { A.constructorTagModifier = normalizeJsonEnumFieldName }

toJsonText :: (A.ToJSON a) => a -> Text
toJsonText = toStrict . A.encodeToLazyText
