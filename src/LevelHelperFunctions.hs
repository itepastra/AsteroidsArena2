module LevelHelperFunctions where

import AsteroidSpawnFunctions (expDecay, expRandom, pow)
import qualified Constants
import Level (InitLevelConfig (..))
import Types1 (Angle, Offset, Part (..), Strength, Var (..))
import VFunctionHelpers (addLinearTerm, collapse)
import VFunctions (VFunction (..), insertAt)
import Wall (InitWall (..))

wallCreatePoly :: Int -> Offset -> Strength -> [InitWall]
wallCreatePoly n o s = map (\x -> InitWall {irFunc = Constant (fromIntegral x * 360 / fromIntegral n), ioFunc = Constant o, isFunc = Constant s}) [1 .. n]

wallModPart :: Part -> (VFunction Float Var -> VFunction Float Var) -> InitWall -> InitWall
wallModPart Str wf iw = iw {isFunc = collapse $ wf (isFunc iw)}
wallModPart Rot wf iw = iw {irFunc = collapse $ wf (irFunc iw)}
wallModPart Off wf iw = iw {ioFunc = collapse $ wf (ioFunc iw)}

wallSetPart :: Part -> VFunction Float Var -> InitWall -> InitWall
wallSetPart Str wf iw = iw {isFunc = wf}
wallSetPart Rot wf iw = iw {irFunc = wf}
wallSetPart Off wf iw = iw {ioFunc = wf}

wallPartMap :: Part -> [VFunction Float Var -> VFunction Float Var] -> [InitWall] -> [InitWall]
wallPartMap = zipWith . wallModPart

wallPartSetMap :: Part -> [VFunction Float Var] -> [InitWall] -> [InitWall]
wallPartSetMap = zipWith . wallSetPart

addRotations :: [Angle] -> [InitWall] -> [InitWall]
addRotations as = wallPartMap Rot (map (addLinearTerm X) as)

addStrengths :: [Strength] -> [InitWall] -> [InitWall]
addStrengths ss = wallPartMap Str (map (addLinearTerm X) ss)

addOffsets :: [Offset] -> [InitWall] -> [InitWall]
addOffsets os = wallPartMap Off (map (addLinearTerm X) os)

defaultLvlConfig :: InitLevelConfig
defaultLvlConfig =
  InitLevelConfig
    { iasteroidSpawnFunction = insertAt Z expDecay expRandom,
      ispaceMineOddsFunction = pow,
      iasteroidSpawnStart = Constants.asteroidSpawnAverageInterval
    }