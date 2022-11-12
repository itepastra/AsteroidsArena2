module DefaultLevels (defaultLevels) where

import qualified Constants
import Data.Fixed (mod')
import Level (GameStateInit (..), InitLevelConfig (..), Level (..))
import LevelHelperFunctions ( defaultLvlConfig, addRotations, wallCreatePoly )
import NoPrereqs (flipFlop)


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
                addRotations (flipFlop (repeat 10)) $
                  wallCreatePoly 4 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "3 - triangle",
        initState =
          GameStateInit
            { initWalls =
                wallCreatePoly 3 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "4 - ManyGon",
        initState =
          GameStateInit
            { initWalls =
                addRotations (flipFlop [1 ..]) $
                  wallCreatePoly 11 400 450,
              initConf = defaultLvlConfig
            }
      },
    Level
      { name = "5 - Twisty Line",
        initState =
          GameStateInit
            { initWalls =
                wallCreatePoly 2 400 450,
              initConf = defaultLvlConfig
            }
      }
  ]