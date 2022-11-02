module LevelHelperFunctions (wallPoly, wallRMap) where

import Rotation (Angle)
import Types1 (Offset, Strength)
import Wall (Wall (Wall, frameRotation), createWall)

wallPoly :: Int -> Offset -> Strength -> [Wall]
wallPoly n o s = map (\x -> createWall o (fromIntegral x * 360 / fromIntegral n) s) [1 .. n]

wallRMap :: [Angle] -> [Wall] -> [Wall]
wallRMap = zipWith (\r w -> w {frameRotation = r})