{-# LANGUAGE InstanceSigs #-}

module VectorCalc where

import FISQ (fisqrt)
import Types1 (Point (..))
import Prelude hiding (negate)

class V2Math a where
  x :: a -> Float
  y :: a -> Float
  (|+|) :: V2Math b => a -> b -> a
  vmap :: (Float -> Float) -> a -> a
  negate :: a -> a
  fromTuple :: (Float, Float) -> a

(|-|) :: (V2Math b, V2Math a) => a -> b -> a
a |-| b = a |+| negate a

(|.|) :: (V2Math b, V2Math a) => a -> b -> Float
a |.| b = x a * x b + y a * y b

(|#|) :: (V2Math b, V2Math a) => a -> b -> Float
a |#| b = c |.| c where c = a |-| b

(|*|) :: V2Math a => Float -> a -> a
a |*| v = vmap (a *) v

toTuple :: V2Math a => a -> (Float, Float)
toTuple v = (x v, y v)

normalize :: V2Math a => a -> a
normalize a = fisqrt (a |.| a) |*| a

instance V2Math Point where
  x :: Point -> Float
  x (Point a _) = a

  y :: Point -> Float
  y (Point _ a) = a

  (|+|) :: V2Math b => Point -> b -> Point
  a |+| b = Point (x a + x b) (y a + y b)

  fromTuple :: (Float, Float) -> Point
  fromTuple (a, b) = Point a b
  negate a = Point (-x a) (-y a)

  vmap :: (Float -> Float) -> Point -> Point
  vmap f (Point x y) = Point (f x) (f y)
