{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GameWall where

import GeneralHelperFunctions (translateP)
import Pictured (Pictured (..), rotWithRot, translate)
import PointHelpers (yUnit)
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseWall)
import TypeClasses (HasA (..))
import Types1 (ElapsedTime, Normal, Offset, Strength)
import VectorCalc ((|*|))
import Point (Point)

data Wall = Wall
  { offset :: Offset,
    strength :: Strength,
    angle :: Angle,
    rFunc :: ElapsedTime -> Angle,
    oFunc :: ElapsedTime -> Offset,
    sFunc :: ElapsedTime -> Strength
  }

instance HasA (ElapsedTime -> Angle, ElapsedTime -> Offset, ElapsedTime -> Strength) Wall where
  getA w = (rFunc w, oFunc w, sFunc w)
  setA (irf, iof, isf) iw = iw {rFunc = irf, oFunc = iof, sFunc = isf}

instance HasA (Angle, Offset, Strength) Wall where
  getA w = (angle w, offset w, strength w)
  setA (a, o, s) w = w {angle = a, offset = o, strength = s}

instance Rotate Wall where
  rotate a w = w {angle = angle w + a}
  getAngle = angle

instance Pictured Wall where
  getPicture w = translateP (point w) $ rotWithRot w baseWall

point :: Wall -> Point Float
point w = (-offset w) |*| normal w

normal :: Wall -> Normal
normal w = rot (angle w) yUnit

setOffset :: Offset -> Wall -> Wall
setOffset o w = w {offset = o}

setRotation :: Angle -> Wall -> Wall
setRotation a w = w {angle = a}

setStrength :: Strength -> Wall -> Wall
setStrength s w = w {strength = s}

fromOAS :: (Offset, Angle, Strength) -> Wall -> Wall
fromOAS (o, a, s) = setOffset o . setRotation a . setStrength s

selfMove :: ElapsedTime -> Wall -> Wall
selfMove et w = fromOAS (oFunc w et, rFunc w et, sFunc w et) w