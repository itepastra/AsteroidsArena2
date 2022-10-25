module Physics where

import TypeClasses
import VectorCalc (Point, Vector)

type TimeStep = Float

type Position = Point

type Acceleration = Vector

type Velocity = Vector

type Dist = Float

type Collides = Bool

data PhysicsObject = PhysObj
  { position :: Position,
    velocity :: Velocity,
    radius :: Float
  }

class HasPhysics a where
  getPhysObj :: a -> PhysicsObject
  setPhysObj :: a -> PhysicsObject -> a

updatePhysObj :: HasPhysics a => (PhysicsObject -> PhysicsObject) -> a -> a
updatePhysObj f p = setPhysObj p $ f $ getPhysObj p

move :: TimeStep -> PhysicsObject -> PhysicsObject
move dt (PhysObj p v r) = PhysObj (p |+| (dt |*| v)) v r

accelerate :: TimeStep -> Acceleration -> PhysicsObject -> PhysicsObject
accelerate dt a (PhysObj p v r) = PhysObj p (v |+| (dt |*| a)) r

friction :: Float -> TimeStep -> PhysicsObject -> PhysicsObject
friction m dt p = p {velocity = (m ** dt) |*| velocity p}

checkCollision :: (HasPhysics a, HasPhysics b) => a -> b -> Collides
checkCollision p b = position pp |#| position pb <= r2
  where
    pp = getPhysObj p
    pb = getPhysObj b
    r = radius pp + radius pb
    r2 = r * r

moveStep :: HasPhysics a => TimeStep -> a -> a
moveStep secs = updatePhysObj (move secs)

accelStep :: HasPhysics a => TimeStep -> Acceleration -> a -> a
accelStep secs a = updatePhysObj $ accelerate secs a 

