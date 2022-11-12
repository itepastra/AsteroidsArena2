{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wall (Wall (..), InitWall (..), selfMove, totalAcceleration, createWall, point, normal) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (singleton)
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import GHC.Read (Read (readPrec))
import Graphics.Gloss (translate)
import qualified Graphics.Gloss as Gloss
import Physics (PhysicsObject (..))
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseWall)
import TypeClasses (HasA (..), HasPhysics (..), Pictured (..), V2Math (..))
import Types1 (Acceleration, ElapsedTime, InWall, Normal, Offset, Point (Point), Strength, X (X))
import VFunctions (VFunction, mkNumFunc)
import VectorCalc ()

data Wall = Wall
  { offset :: Offset,
    strength :: Strength,
    angle :: Angle,
    rFunc :: ElapsedTime -> Angle,
    oFunc :: ElapsedTime -> Offset,
    sFunc :: ElapsedTime -> Strength
  }

data InitWall = InitWall
  { irFunc :: VFunction X Float,
    ioFunc :: VFunction X Float,
    isFunc :: VFunction X Float
  }

instance Show InitWall where
  show w = "Rotation: " ++ show (irFunc w) ++ " Offset: " ++ show (ioFunc w) ++ " Strength: " ++ show (isFunc w) ++ "\n"

instance HasA (VFunction X Float, VFunction X Float, VFunction X Float) InitWall where
  getA w = (irFunc w, ioFunc w, isFunc w)
  setA (irf, iof, isf) iw = iw {irFunc = irf, ioFunc = iof, isFunc = isf}

instance HasA (ElapsedTime -> Angle, ElapsedTime -> Offset, ElapsedTime -> Strength) Wall where
  getA w = (rFunc w, oFunc w, sFunc w)
  setA (irf, iof, isf) iw = iw {rFunc = irf, oFunc = iof, sFunc = isf}

instance HasA (Angle, Offset, Strength) Wall where
  getA w = (angle w, offset w, strength w)
  setA (a, o, s) w = w {angle = a, offset = o, strength = s}

point :: Wall -> Point
point w = (-offset w) |*| normal w

normal :: Wall -> Normal
normal w = rot (angle w) (Point 0 1)

instance Rotate Wall where
  rotate a w = w {angle = angle w + a}
  getAngle = angle

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
    coFunc = mkNumFunc (ioFunc iw) . singleton X
    crFunc = mkNumFunc (irFunc iw) . singleton X
    csFunc = mkNumFunc (isFunc iw) . singleton X

toOAS :: Wall -> (Offset, Angle, Strength)
toOAS w = (offset w, angle w, strength w)

fromOAS :: (Offset, Angle, Strength) -> Wall -> Wall
fromOAS (o, a, s) = setOffset o . setRotation a . setStrength s

fromOASR :: (Offset, Angle, Strength, Angle) -> Wall -> Wall
fromOASR (o, a, s, r) = fromOAS (o, a, s)

selfMove :: ElapsedTime -> Wall -> Wall
selfMove et w = fromOAS (oFunc w et, rFunc w et, sFunc w et) w
