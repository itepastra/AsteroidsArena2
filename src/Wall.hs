module Wall where

import Data.Maybe (mapMaybe)
import GHC.Read (Read (readPrec))
import Graphics.Gloss (translate)
import qualified Graphics.Gloss as Gloss
import Rotation (Rotate (..), rot)
import Sprites (baseWall)
import TypeClasses (Pictured (..), V2Math (..), HasPhysics (..))
import Types1
    ( Acceleration,
      ElapsedTime,
      InWall,
      Normal,
      Offset,
      Strength,
      TimeStep,
      Angle,
      Point(Point),
      Point, PhysicsObject (..) )
import VectorCalc ()
import AFunctions (AFunction, createFunc)

data Wall = Wall
  { offset :: Offset,
    strength :: Strength,
    angle :: Angle,
    rFunc :: ElapsedTime -> Angle,
    oFunc :: ElapsedTime -> Offset,
    sFunc :: ElapsedTime -> Strength
  } 

data InitWall = InitWall
  { irFunc :: AFunction,
    ioFunc :: AFunction,
    isFunc :: AFunction
  } deriving (Show)

point :: Wall -> Point
point w = (-offset w) |*| normal w

normal :: Wall -> Normal
normal w = rot (angle w) (Point 0 1)

instance Rotate Wall where
  rotate a w = w {angle = angle w + a}

instance Pictured Wall where
  getPicture w = translate (x p) (y p) $ Gloss.Rotate (-a + 180) baseWall
    where
      p = point w
      a = angle w

isInWall :: HasPhysics a => a -> Wall -> InWall
isInWall obj w = (pos |-| p) |.| n <= 0
  where
    pos = (position . getPhysObj) obj
    p = point w
    n = normal w

wallAcceleration :: HasPhysics a => a -> Wall -> Maybe Acceleration
wallAcceleration o w
  | isInWall o w = Just (strength w |*| normal w)
  | otherwise = Nothing

totalAcceleration :: HasPhysics a => [Wall] -> a -> Acceleration
totalAcceleration ws o = foldr (|+|) (Point 0 0) (mapMaybe (wallAcceleration o) ws)

setOffset :: Offset -> Wall -> Wall
setOffset o w = w {offset = o}

setRotation :: Angle -> Wall -> Wall
setRotation a w = w {angle = a}

setStrength :: Strength -> Wall -> Wall
setStrength s w = w {strength = s}

createWall :: InitWall -> Wall
createWall iw = Wall {offset = coFunc 0, angle = crFunc 0, strength = csFunc 0, oFunc = coFunc, rFunc = crFunc, sFunc = csFunc}
  where
    coFunc = createFunc (ioFunc iw)
    crFunc = createFunc (irFunc iw)
    csFunc = createFunc (isFunc iw)

toOAS :: Wall -> (Offset, Angle, Strength)
toOAS w = (offset w, angle w, strength w)

fromOAS :: (Offset, Angle, Strength) -> Wall -> Wall
fromOAS (o, a, s) = setOffset o . setRotation a . setStrength s

fromOASR :: (Offset, Angle, Strength, Angle) -> Wall -> Wall
fromOASR (o, a, s, r) = fromOAS (o, a, s)

selfMove :: ElapsedTime -> Wall -> Wall
selfMove et w = fromOAS (oFunc w et, rFunc w et, sFunc w et) w