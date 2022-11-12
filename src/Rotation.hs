module Rotation (rot, Angle, Rotate (..)) where

import PointHelpers (zeroPoint)
import Types1 (Angle)
import Prelude hiding (negate)
import Point (Point)
import VectorCalc (fromTuple, y, x, toTuple)

rot ::  (Eq a, Floating a) =>  a -> Point a -> Point a
rot 0 v = v
rot 90 v = fromTuple (-y v, x v)
rot (-90) v = fromTuple (y v, -x v)
rot 180 v = -v
rot r v = fromTuple (x * cs - y * sn, x * sn + y * cs)
  where
    (x, y) = toTuple v
    th = r * pi / 180
    cs = cos th
    sn = sin th

class Rotate a where
  rotate :: Angle -> a -> a
  getAngle :: a -> Angle