module Wall where

import Physics (HasPhysics (physobj), PhysicsObject (position))
import VectorCalc (Point, Vector, (|-|), (|.|))

type Normal = Vector

data Wall = Wall Point Normal

type InWall = Bool

isInWall :: HasPhysics a => a -> Wall -> InWall
isInWall obj (Wall p n) = (pos |-| p) |.| n <= 0
  where
    pos = (position . physobj) obj
