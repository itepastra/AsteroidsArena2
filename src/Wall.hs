module Wall where

import Data.Maybe (mapMaybe)
import Physics (Acceleration, HasPhysics (physobj), PhysicsObject (position))
import Rotation (Rotate (..), rot)
import VectorCalc (Point (Point), V2Math (..), Vector, (|-|), (|.|))

type Normal = Vector

type Strength = Float

data Wall = Wall
  { point :: Point,
    normal :: Normal,
    strength :: Strength
  }

type InWall = Bool

isInWall :: HasPhysics a => a -> Wall -> InWall
isInWall obj (Wall p n _) = (pos |-| p) |.| n <= 0
  where
    pos = (position . physobj) obj

wallAcceleration :: HasPhysics a => a -> Wall -> Maybe Acceleration
wallAcceleration o w
  | isInWall o w = Just (strength w |*| normal w)
  | otherwise = Nothing

totalAcceleration :: HasPhysics a => a -> [Wall] -> Acceleration
totalAcceleration o ws = foldr (|+|) (Point 0 0) (mapMaybe (wallAcceleration o) ws)

instance Rotate Wall where
  rotate a w = Wall {point = rot a (point w), normal = rot a (normal w), strength = strength w}