{-# LANGUAGE OverloadedStrings #-}

module JSONfuncs (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=)) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Types1 (Point (..))
import VectorCalc (x, y)

instance FromJSON Point where
  parseJSON = withObject "Point" $ \v ->
    Point
      <$> v
      .: "x"
      <*> v
      .: "y"

instance ToJSON Point where
  toJSON p =
    object
      [ "x" .= x p,
        "y" .= y p
      ]
