{-# LANGUAGE InstanceSigs #-}
module VectorCalc where

import Prelude hiding (negate)
import TypeClasses (V2Math (..))


data Point = Point Float Float

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

  normalize :: Point -> Point
  normalize a = (1 / sqrt (a |.| a)) |*| a

  vmap :: (Float -> Float) -> Point -> Point
  vmap f (Point x y) = Point (f x) (f y)
