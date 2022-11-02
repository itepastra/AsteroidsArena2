module DefaultLevels (defaultLevels) where

import AsteroidSpawnFunctions (DecayFunctions (..), MapFunctions (..), RandomFunctions (..))
import qualified Constants
import Level (InitLevelConfig (..), Level (..), GameStateInit (..))
import Wall (Wall (frameRotation), createWall)
import Types1 (Offset, Strength)

defaultLvlConfig :: InitLevelConfig
defaultLvlConfig =
  InitLevelConfig
    { iasteroidSpawnFunction = ExpRandom,
      iasteroidDecayFunction = ExpDecay,
      ispaceMineOddsFunction = Pow,
      iasteroidSpawnStart = Constants.asteroidSpawnAverageInterval
    }

wallPoly :: Int -> Offset -> Strength -> [Wall]
wallPoly n o s = map (\x -> createWall o (fromIntegral x * 360 / fromIntegral n) s) [1 .. n]

defaultLevels :: [Level]
defaultLevels =
  [ Level
      { name = "1 - empty",
        initState =
          GameStateInit
            { initWalls =
                [],
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "2 - box",
        initState =
          GameStateInit
            { initWalls =
                wallPoly 4 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "3 - triangle",
        initState =
          GameStateInit
            { initWalls =
                wallPoly 3 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "4 - ManyGon",
        initState =
          GameStateInit
            { initWalls =
                zipWith (\r w -> w {frameRotation = r}) [1 ..] $
                  wallPoly 11 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "5 - Twisty Line",
        initState =
          GameStateInit
            { initWalls =
                wallPoly 2 400 450,
              initConf = defaultLvlConfig
            }
      }
  ]