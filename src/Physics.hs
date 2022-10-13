
module Physics where

import VectorCalc (Point, V2Math ((|*|), (|+|)), Vector, (|#|))

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
  physobj :: a -> PhysicsObject
  moveStep :: a -> TimeStep -> a
  accelStep :: a -> TimeStep -> Acceleration -> a

move :: PhysicsObject -> TimeStep -> PhysicsObject
move (PhysObj p v r) dt = PhysObj (p |+| (dt |*| v)) v r

accelerate :: PhysicsObject -> TimeStep -> Acceleration -> PhysicsObject
accelerate (PhysObj p v r) dt a = PhysObj p (v |+| (dt |*| a)) r

checkCollision :: (HasPhysics a, HasPhysics b) => a -> b -> Collides
checkCollision p b = position pp |#| position pb <= r2
  where
    pp = physobj p
    pb = physobj b
    r = radius pp + radius pb
    r2 = r * r
