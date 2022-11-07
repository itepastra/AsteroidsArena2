module Physics where

import TypeClasses ( V2Math((|#|), (|+|), (|*|)) )
import Types1 (TimeStep, Position, Velocity, Acceleration, Collides)
import VectorCalc ()



data PhysicsObject = PhysObj
  { position :: Position,
    velocity :: Velocity,
    radius :: Float
  }

class HasPhysics a where
  getPhysObj :: a -> PhysicsObject
  setPhysObj :: PhysicsObject ->a ->  a

updatePhysObj :: HasPhysics a => (PhysicsObject -> PhysicsObject) -> a -> a
updatePhysObj f p = setPhysObj (f $ getPhysObj p) p

move :: TimeStep -> PhysicsObject -> PhysicsObject
move dt (PhysObj p v r) = PhysObj (p |+| (dt |*| v)) v r

accelerate :: TimeStep -> Acceleration -> PhysicsObject -> PhysicsObject
accelerate dt a (PhysObj p v r) = PhysObj p (v |+| (dt |*| a)) r

friction :: Float -> TimeStep -> PhysicsObject -> PhysicsObject
friction m dt p = p {velocity = (m ** dt) |*| velocity p}

moveStep :: HasPhysics a => TimeStep -> a -> a
moveStep secs = updatePhysObj (move secs)

accelStep :: HasPhysics a => TimeStep -> Acceleration -> a -> a
accelStep secs a = updatePhysObj $ accelerate secs a

frictionStep :: HasPhysics a => Float -> TimeStep -> a -> a
frictionStep exp secs = updatePhysObj $ friction exp secs

checkCollision :: (HasPhysics a, HasPhysics b) => a -> b -> Collides
checkCollision p b = position pp |#| position pb <= r2
  where
    pp = getPhysObj p
    pb = getPhysObj b
    r = radius pp + radius pb
    r2 = r * r
