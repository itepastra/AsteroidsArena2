{-# LANGUAGE OverloadedStrings #-}

module Point where

import Control.Applicative (Applicative (liftA2))
import GHC.Read (Read (..), expectP)
import JSONfuncs
import qualified Text.Read.Lex as L

data Point a = Point a a

instance Functor Point where
  fmap f (Point x y) = Point (f x) (f y)

instance Applicative Point where
  pure v = Point v v
  (Point f1 f2) <*> (Point x y) = Point (f1 x) (f2 y)

instance Num a => Num (Point a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger
  (-) = liftA2 (-)

instance Fractional a => Fractional (Point a) where
  fromRational = pure . fromRational
  (/) = liftA2 (/)

instance Floating a => Floating (Point a) where
  pi = pure pi
  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance Semigroup a => Semigroup (Point a) where
  (Point x1 y1) <> (Point x2 y2) = Point (x1 <> x2) (y1 <> y2)

instance Monoid a => Monoid (Point a) where
  mempty = Point mempty mempty

instance Show a => Show (Point a) where
  show (Point x y) = show (x, y)

instance Read a => Read (Point a) where
  readPrec = uncurry Point <$> readPrec

instance Foldable Point where
  foldMap m (Point a b) = m a <> m b

instance Traversable Point where
  traverse f (Point x y) = Point <$> f x <*> f y

instance Eq a => Eq (Point a) where
  (Point a1 b1) == (Point a2 b2) = a1 == a2 && b1 == b2

instance Bounded a => Bounded (Point a) where
  minBound = Point minBound minBound
  maxBound = Point maxBound maxBound

instance FromJSON a => FromJSON (Point a) where
  parseJSON = withObject "Point" $ \v ->
    Point
      <$> v
      .: "x"
      <*> v
      .: "y"

instance ToJSON a => ToJSON (Point a) where
  toJSON (Point x y) =
    object
      [ "x" .= x,
        "y" .= y
      ]
