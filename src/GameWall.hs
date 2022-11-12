{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GameWall where
import Types1 (Offset, Strength, ElapsedTime, Point, Normal)
import Rotation ( Angle, rot, Rotate (..) )
import TypeClasses (HasA (..))
import VectorCalc ( (|*|) )
import PointHelpers ( yUnit )

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

point :: Wall -> Point
point w = (-offset w) |*| normal w

normal :: Wall -> Normal
normal w = rot (angle w) yUnit

instance Rotate Wall where
  rotate a w = w {angle = angle w + a}
  getAngle = angle

setOffset :: Offset -> Wall -> Wall
setOffset o w = w {offset = o}

setRotation :: Angle -> Wall -> Wall
setRotation a w = w {angle = a}

setStrength :: Strength -> Wall -> Wall
setStrength s w = w {strength = s}

fromOAS :: (Offset, Angle, Strength) -> Wall -> Wall
fromOAS (o, a, s) = setOffset o . setRotation a . setStrength s

fromOASR :: (Offset, Angle, Strength, Angle) -> Wall -> Wall
fromOASR (o, a, s, r) = fromOAS (o, a, s)

selfMove :: ElapsedTime -> Wall -> Wall
selfMove et w = fromOAS (oFunc w et, rFunc w et, sFunc w et) w