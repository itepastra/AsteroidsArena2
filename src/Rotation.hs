module Rotation (rot, Angle, Rotate (..)) where

import TypeClasses (V2Math (..))
import Types1 (Angle)

rot :: (V2Math a) => Angle -> a -> a
rot 0 v = v
rot 90 v = fromTuple (-y v, x v)
rot (-90) v = fromTuple (y v, -x v)
rot 180 v = TypeClasses.negate v
rot r v = fromTuple (x * cs - y * sn, x * sn + y * cs)
  where
    (x, y) = toTuple v
    th = r * pi / 180
    cs = cos th
    sn = sin th

class Rotate a where
  rotate :: Angle -> a -> a
  getAngle :: a -> Angle