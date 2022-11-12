{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Physics (updatePhysObj, moveStep, accelStep, frictionStep, PhysicsObject (..), checkCollision) where

import TypeClasses (HasPhysics (..), HasA(..))
import Types1 (Acceleration, Collides, PhysicsObject (..), TimeStep)
import VectorCalc ( V2Math((|+|)), (|#|), (|*|) )

instance HasPhysics a => HasA PhysicsObject a where
  getA = getPhysObj
  setA = setPhysObj

updatePhysObj :: HasA PhysicsObject a => (PhysicsObject -> PhysicsObject) -> a -> a
updatePhysObj = updateA

move :: TimeStep -> PhysicsObject -> PhysicsObject
move dt (PhysObj p v r) = PhysObj (p |+| (dt |*| v)) v r

accelerate :: TimeStep -> Acceleration -> PhysicsObject -> PhysicsObject
accelerate dt a (PhysObj p v r) = PhysObj p (v |+| (dt |*| a)) r

friction :: Float -> TimeStep -> PhysicsObject -> PhysicsObject
friction m dt p = p {velocity = (m ** dt) |*| velocity p}

moveStep :: HasA PhysicsObject a => TimeStep -> a -> a
moveStep secs = updatePhysObj (move secs)

accelStep :: HasA PhysicsObject a => TimeStep -> Acceleration -> a -> a
accelStep secs a = updatePhysObj $ accelerate secs a

frictionStep :: HasA PhysicsObject a => Float -> TimeStep -> a -> a
frictionStep exp secs = updatePhysObj $ friction exp secs

checkCollision :: (HasA PhysicsObject a, HasA PhysicsObject b) => a -> b -> Collides
checkCollision p b = position pp |#| position pb <= r2
  where
    pp = getA p
    pb = getA b
    r = radius pp + radius pb
    r2 = r * r
