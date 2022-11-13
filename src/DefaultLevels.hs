module DefaultLevels (defaultLevels) where

import qualified Constants
import Data.Fixed (mod')
import GeneralHelperFunctions (flipFlop)
import Level (GameStateInit (..), InitLevelConfig (..), Level (..))
import LevelHelperFunctions (addRotations, defaultLvlConfig, wallCreatePoly)

defaultLevels :: [Level]
defaultLevels =
  [ Level
      { name = "empty",
        initState =
          GameStateInit
            { initWalls =
                [],
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "weird box",
        initState =
          GameStateInit
            { initWalls =
                addRotations (flipFlop (repeat 10)) $
                  wallCreatePoly 4 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "triangle",
        initState =
          GameStateInit
            { initWalls =
                wallCreatePoly 3 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "manygon",
        initState =
          GameStateInit
            { initWalls =
                addRotations (flipFlop [1 ..]) $
                  wallCreatePoly 11 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "twisty line",
        initState =
          GameStateInit
            { initWalls =
                wallCreatePoly 2 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "Death",
        initState =
          GameStateInit
            { initWalls =
                wallCreatePoly 2 400 450,
              initConf = defaultLvlConfig {iasteroidSpawnStart = 0.1}
            }
      }
  ]