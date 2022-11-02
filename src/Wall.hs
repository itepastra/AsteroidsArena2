module Wall where

import Data.Maybe (mapMaybe)
import GHC.Read (Read (readPrec))
import Graphics.Gloss (translate)
import qualified Graphics.Gloss as Gloss
import Physics (HasPhysics (getPhysObj), PhysicsObject (position))
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseWall)
import TypeClasses (Pictured (..), V2Math (..))
import Types1 (Acceleration, InWall, Normal, Offset, Strength, TimeStep)
import VectorCalc (Point (Point))


data Wall = Wall
  { point :: Point,
    normal :: Normal,
    strength :: Strength,
    angle :: Angle,
    frameRotation :: Angle
  }

instance Rotate Wall where
  rotate a w = w {point = rot a (point w), normal = rot a (normal w), angle = angle w + a}

instance Pictured Wall where
  getPicture (Wall {point = p, normal = n, angle = r}) = translate (x p) (y p) $ Gloss.Rotate (-r + 180) baseWall

isInWall :: HasPhysics a => a -> Wall -> InWall
isInWall obj (Wall {point = p, normal = n}) = (pos |-| p) |.| n <= 0
  where
    pos = (position . getPhysObj) obj

wallAcceleration :: HasPhysics a => a -> Wall -> Maybe Acceleration
wallAcceleration o w
  | isInWall o w = Just (strength w |*| normal w)
  | otherwise = Nothing

totalAcceleration :: HasPhysics a => [Wall] -> a -> Acceleration
totalAcceleration ws o = foldr (|+|) (Point 0 0) (mapMaybe (wallAcceleration o) ws)

setOffset :: Offset -> Wall -> Wall
setOffset o w = w {point = (-o) |*| normal w}

setRotation :: Angle -> Wall -> Wall
setRotation a w = w {point = rot a (Point 0 (-1)), normal = rot a (Point 0 1), angle = a}

setStrength :: Strength -> Wall -> Wall
setStrength s w = w {strength = s}

baseW :: Wall
baseW = Wall {point = Point 0 (-1), normal = Point 0 1, angle = 0, strength = 1, frameRotation = 0}

createWall :: Offset -> Angle -> Strength -> Wall
createWall o a s = (setOffset o . setRotation a . setStrength s) baseW

toOAS :: Wall -> (Offset, Angle, Strength)
toOAS w = (sqrt (point w |.| point w), angle w, strength w)

fromOAS :: (Offset, Angle, Strength) -> Wall
fromOAS (o, a, s) = createWall o a s

fromOASR :: (Offset, Angle, Strength, Angle) -> Wall
fromOASR (o,a,s,r) = (fromOAS (o,a,s)) {frameRotation = r}

selfRotate :: TimeStep -> Wall -> Wall
selfRotate  s = rotate . (s *) =<< frameRotation