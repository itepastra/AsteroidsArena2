module LevelHelperFunctions where

import Types1 (Offset, Strength, Angle)
import Wall (InitWall (..))
import AFunctions (AFunction (..))

wallPoly :: Int -> Offset -> Strength -> [InitWall]
wallPoly n o s = map (\x -> InitWall {irFunc = C (fromIntegral x * 360 / fromIntegral n), ioFunc = C o, isFunc = C s}) [1 .. n]

data Part = Str | Rot | Off

modPart :: Part -> (AFunction -> AFunction) -> InitWall -> InitWall
modPart Str wf iw = iw {isFunc = wf (isFunc iw)}
modPart Rot wf iw = iw {irFunc = wf (irFunc iw)}
modPart Off wf iw = iw {ioFunc = wf (ioFunc iw)}

setPart :: Part -> AFunction -> InitWall -> InitWall
setPart Str wf iw = iw {isFunc = wf}
setPart Rot wf iw = iw {irFunc = wf}
setPart Off wf iw = iw {ioFunc = wf}

lin :: (AFunction -> AFunction -> AFunction) -> Float -> AFunction -> AFunction
lin w f = w (MulF (C f) Etime)

wallPartMap :: Part -> [AFunction -> AFunction] -> [InitWall] -> [InitWall]
wallPartMap = zipWith . modPart

wallPartSetMap :: Part -> [AFunction] -> [InitWall] -> [InitWall]
wallPartSetMap = zipWith . setPart

flipFlop :: [Float] -> [Float]
flipFlop = zipWith (*) (concat $ repeat [1, -1])

makeConstants :: [Float] -> [AFunction]
makeConstants = map C

addRots :: [Angle] -> [InitWall] -> [InitWall]
addRots as = wallPartMap Rot (map (lin AddF) as)

addStrengts :: [Strength] -> [InitWall] -> [InitWall]
addStrengts ss = wallPartMap Str (map (lin AddF) ss)

addOffsets :: [Offset] -> [InitWall] -> [InitWall]
addOffsets os = wallPartMap Off (map (lin AddF) os)
