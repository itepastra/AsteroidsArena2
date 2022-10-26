module Wall where

import Data.Maybe (mapMaybe)
import GHC.Read (Read (readPrec))
import Graphics.Gloss (translate)
import qualified Graphics.Gloss as Gloss
import Physics (Acceleration, HasPhysics (getPhysObj), PhysicsObject (position))
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseWall)
import TypeClasses (Pictured (..), V2Math (..))
import VectorCalc (Point (Point), Vector)

type Normal = Vector

type Strength = Float

type InWall = Bool

data Wall = Wall
  { point :: Point,
    normal :: Normal,
    strength :: Strength,
    angle :: Angle
  }

instance Rotate Wall where
  rotate a w = Wall {point = rot a (point w), normal = rot a (normal w), strength = strength w, angle = angle w + a}

instance Pictured Wall where
  getGlobalPicture (Wall p n _ r) = translate (x p) (y p) $ Gloss.Rotate (-r + 180) baseWall

isInWall :: HasPhysics a => a -> Wall -> InWall
isInWall obj (Wall p n _ _) = (pos |-| p) |.| n <= 0
  where
    pos = (position . getPhysObj) obj

wallAcceleration :: HasPhysics a => a -> Wall -> Maybe Acceleration
wallAcceleration o w
  | isInWall o w = Just (strength w |*| normal w)
  | otherwise = Nothing

totalAcceleration :: HasPhysics a => [Wall] -> a -> Acceleration
totalAcceleration ws o = foldr (|+|) (Point 0 0) (mapMaybe (wallAcceleration o) ws)
