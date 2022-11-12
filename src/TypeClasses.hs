{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeClasses where

import FISQ (fisqrt)
import Graphics.Gloss (Picture)
import Types1 (PhysicsObject)
import Prelude hiding (negate)

class Pictured a where
  getPicture :: a -> Picture

class HasPhysics a where
  getPhysObj :: a -> PhysicsObject
  setPhysObj :: PhysicsObject -> a -> a

infixr 9 #

class HasA a b where
  getA :: b -> a
  setA :: a -> b -> b
  updateA :: HasA a b => (a -> a) -> b -> b
  updateA f a = setA (f (getA a)) a
  (#) :: HasA a b => (a -> a) -> b -> b
  (#) = updateA
