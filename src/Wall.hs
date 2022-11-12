{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wall (Wall (..), InitWall (..), selfMove, totalAcceleration, createWall, point, normal) where

import GameWall (Wall (..), point, normal, selfMove)
import TypeClasses (HasPhysics (..))
import Types1
    ( InWall,
      Acceleration,
      Offset,
      Angle,
      Strength,
      ElapsedTime,
      PhysicsObject(..), Var (X) )
import InitWall (InitWall (..))
import VectorCalc ( (|*|), (|.|) ) 
import Data.Maybe (mapMaybe)
import PointHelpers (zeroPoint)
import Data.Map (singleton)
import VFunctions (mkNumFunc)



isInWall :: HasPhysics a => a -> Wall -> InWall
isInWall obj w = (pos - p) |.| n <= 0
  where
    pos = (position . getPhysObj) obj
    p = point w
    n = normal w

wallAcceleration :: HasPhysics a => a -> Wall -> Maybe Acceleration
wallAcceleration o w
  | isInWall o w = Just (strength w |*| normal w)
  | otherwise = Nothing

totalAcceleration :: HasPhysics a => [Wall] -> a -> Acceleration
totalAcceleration ws o = foldr (+) zeroPoint (mapMaybe (wallAcceleration o) ws)


createWall :: InitWall -> Wall
createWall iw = Wall {offset = coFunc 0, angle = crFunc 0, strength = csFunc 0, oFunc = coFunc, rFunc = crFunc, sFunc = csFunc}
  where
    coFunc = mkNumFunc (ioFunc iw) . singleton X
    crFunc = mkNumFunc (irFunc iw) . singleton X
    csFunc = mkNumFunc (isFunc iw) . singleton X

toOAS :: Wall -> (Offset, Angle, Strength)
toOAS w = (offset w, angle w, strength w)

