{-# LANGUAGE InstanceSigs #-}

module VectorCalc where

import Prelude hiding (negate)

-- import Graphics.Gloss.Data.Point (Point)
-- import Graphics.Gloss.Data.Vector (Vector)

data Point = Point Float Float

type Vector = Point

class V2Math a where
  x :: a -> Float
  y :: a -> Float
  (|+|) :: V2Math b => a -> b -> a
  (|*|) :: Float -> a -> a
  (|-|) :: V2Math b => a -> b -> a
  fromTuple :: (Float, Float) -> a
  toTuple :: a -> (Float, Float)
  negate :: a -> a
  (|.|) :: V2Math b => a -> b -> Float
  (|#|) :: V2Math b => a -> b -> Float

instance V2Math Point where
  x :: Point -> Float
  x (Point a _) = a
  y :: Point -> Float
  y (Point _ a) = a
  (|+|) :: V2Math b => Point -> b -> Point
  a |+| b = Point (x a + x b) (y a + y b)
  (|*|) :: Float -> Point -> Point
  m |*| a = Point (m * x a) (m * y a)
  fromTuple :: (Float, Float) -> Point
  fromTuple (a, b) = Point a b
  toTuple :: Point -> (Float, Float)
  toTuple (Point a b) = (a, b)
  negate :: Point -> Point
  negate a = Point (-x a) (-y a)
  (|-|) :: V2Math b => Point -> b -> Point
  a |-| b = a |+| negate b
  (|.|) :: V2Math b => Point -> b -> Float
  a |.| b = x a * x b + y a * y b
  (|#|) :: V2Math b => Point -> b -> Float
  a |#| b = c |.| c where c = a |-| b
