module TypeClasses where

import Graphics.Gloss (Picture)
import Types1 (PhysicsObject)

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
  normalize :: a -> a
  vmap :: (Float -> Float) -> a -> a

class Pictured a where
  getPicture :: a -> Picture

class HasPhysics a where
  getPhysObj :: a -> PhysicsObject
  setPhysObj :: PhysicsObject -> a -> a