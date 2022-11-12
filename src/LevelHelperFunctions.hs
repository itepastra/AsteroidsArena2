module LevelHelperFunctions where

import AsteroidSpawnFunctions (expDecay, expRandom, pow)
import qualified Constants
import Level (InitLevelConfig (..))
import Types1 (Angle, Offset, Part (..), Strength, Var (..))
import VFunctions (VFunction (..), collapse, insertAt)
import Wall (InitWall (..))

wallPoly :: Int -> Offset -> Strength -> [InitWall]
wallPoly n o s = map (\x -> InitWall {irFunc = Constant (fromIntegral x * 360 / fromIntegral n), ioFunc = Constant o, isFunc = Constant s}) [1 .. n]

modPart :: Part -> (VFunction Float Var -> VFunction Float Var) -> InitWall -> InitWall
modPart Str wf iw = iw {isFunc = collapse $ wf (isFunc iw)}
modPart Rot wf iw = iw {irFunc = collapse $ wf (irFunc iw)}
modPart Off wf iw = iw {ioFunc = collapse $ wf (ioFunc iw)}

setPart :: Part -> VFunction Float Var -> InitWall -> InitWall
setPart Str wf iw = iw {isFunc = wf}
setPart Rot wf iw = iw {irFunc = wf}
setPart Off wf iw = iw {ioFunc = wf}

addLinearTerm :: (VFunction Float Var -> VFunction Float Var -> VFunction Float Var) -> Float -> VFunction Float Var -> VFunction Float Var
addLinearTerm op x f = op f (Constant x * Variable X)

wallPartMap :: Part -> [VFunction Float Var -> VFunction Float Var] -> [InitWall] -> [InitWall]
wallPartMap = zipWith . modPart

wallPartSetMap :: Part -> [VFunction Float Var] -> [InitWall] -> [InitWall]
wallPartSetMap = zipWith . setPart

addRots :: [Angle] -> [InitWall] -> [InitWall]
addRots as = wallPartMap Rot (map (addLinearTerm (+)) as)

addStrengts :: [Strength] -> [InitWall] -> [InitWall]
addStrengts ss = wallPartMap Str (map (addLinearTerm (+)) ss)

addOffsets :: [Offset] -> [InitWall] -> [InitWall]
addOffsets os = wallPartMap Off (map (addLinearTerm (+)) os)

defaultLvlConfig :: InitLevelConfig
defaultLvlConfig =
  InitLevelConfig
    { iasteroidSpawnFunction = insertAt Z expDecay expRandom,
      ispaceMineOddsFunction = pow,
      iasteroidSpawnStart = Constants.asteroidSpawnAverageInterval
    }