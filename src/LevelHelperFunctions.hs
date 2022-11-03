module LevelHelperFunctions where

import Rotation (Angle)
import Types1 (Offset, Strength)
import Wall (InitWall (..))
import WallFunctions (WallFunction (..))

wallPoly :: Int -> Offset -> Strength -> [InitWall]
wallPoly n o s = map (\x -> InitWall {irFunc = C (fromIntegral x * 360 / fromIntegral n), ioFunc = C o, isFunc = C s}) [1 .. n]

data Part = Str | Rot | Off

modPart :: Part -> (WallFunction -> WallFunction) -> InitWall -> InitWall
modPart Str wf iw = iw {isFunc = wf (isFunc iw)}
modPart Rot wf iw = iw {irFunc = wf (irFunc iw)}
modPart Off wf iw = iw {ioFunc = wf (ioFunc iw)}

setPart :: Part -> WallFunction -> InitWall -> InitWall
setPart Str wf iw = iw {isFunc = wf}
setPart Rot wf iw = iw {irFunc = wf}
setPart Off wf iw = iw {ioFunc = wf}

lin :: (WallFunction -> WallFunction -> WallFunction) -> Float -> WallFunction -> WallFunction
lin w f = w (LinF (C f) Etime)

wallPartMap :: Part -> [WallFunction -> WallFunction] -> [InitWall] -> [InitWall]
wallPartMap = zipWith . modPart

wallPartSetMap :: Part -> [WallFunction] -> [InitWall] -> [InitWall]
wallPartSetMap = zipWith . setPart

flipFlop :: [Float] -> [Float]
flipFlop = zipWith (*) (concat $ repeat [1, -1])

makeConstants :: [Float] -> [WallFunction]
makeConstants = map C

addRots :: [Angle] -> [InitWall] -> [InitWall]
addRots as = wallPartMap Rot (map (lin AddF) as)

addStrengts :: [WallFunction] -> [InitWall] -> [InitWall]
addStrengts = undefined