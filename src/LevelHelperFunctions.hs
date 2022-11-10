module LevelHelperFunctions where

import AFunctions (AFunction (..), collapse)
import AsteroidSpawnFunctions (DecayFunctions (..), MapFunctions (..), RandomFunctions (..))
import qualified Constants
import Level (InitLevelConfig (..))
import Types1 (Angle, Offset, Strength)
import Wall (InitWall (..))

wallPoly :: Int -> Offset -> Strength -> [InitWall]
wallPoly n o s = map (\x -> InitWall {irFunc = C (fromIntegral x * 360 / fromIntegral n), ioFunc = C $ realToFrac o, isFunc = C $ realToFrac s}) [1 .. n]

data Part = Str | Rot | Off

modPart :: Part -> (AFunction Float -> AFunction Float) -> InitWall -> InitWall
modPart Str wf iw = iw {isFunc = collapse $ wf (isFunc iw)}
modPart Rot wf iw = iw {irFunc = collapse $ wf (irFunc iw)}
modPart Off wf iw = iw {ioFunc = collapse $ wf (ioFunc iw)}

setPart :: Part -> AFunction Float -> InitWall -> InitWall
setPart Str wf iw = iw {isFunc = wf}
setPart Rot wf iw = iw {irFunc = wf}
setPart Off wf iw = iw {ioFunc = wf}

lin :: (Real a, Fractional a) => (AFunction a -> AFunction a -> AFunction a) -> a -> AFunction a -> AFunction a
lin w f = w (MulF (C $ realToFrac f) Var)

wallPartMap :: Part -> [AFunction Float -> AFunction Float] -> [InitWall] -> [InitWall]
wallPartMap = zipWith . modPart

wallPartSetMap :: Part -> [AFunction Float] -> [InitWall] -> [InitWall]
wallPartSetMap = zipWith . setPart

flipFlop :: [Float] -> [Float]
flipFlop = zipWith (*) (concat $ repeat [1, -1])

makeConstants :: Real a => [a] -> [AFunction a]
makeConstants = map C

addRots :: [Angle] -> [InitWall] -> [InitWall]
addRots as = wallPartMap Rot (map (lin AddF) as)

addStrengts :: [Strength] -> [InitWall] -> [InitWall]
addStrengts ss = wallPartMap Str (map (lin AddF) ss)

addOffsets :: [Offset] -> [InitWall] -> [InitWall]
addOffsets os = wallPartMap Off (map (lin AddF) os)

defaultLvlConfig :: InitLevelConfig
defaultLvlConfig =
  InitLevelConfig
    { iasteroidSpawnFunction = ExpRandom,
      iasteroidDecayFunction = ExpDecay,
      ispaceMineOddsFunction = Pow,
      iasteroidSpawnStart = Constants.asteroidSpawnAverageInterval
    }