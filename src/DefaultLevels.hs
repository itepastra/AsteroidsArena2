module DefaultLevels (defaultLevels) where

import qualified Constants
import Data.Fixed (mod')
import Level (GameStateInit (..), InitLevelConfig (..), Level (..))
import LevelHelperFunctions ( defaultLvlConfig, addRots, wallPoly )
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
                addRots (flipFlop (repeat 10)) $
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
                addRots (flipFlop [1 ..]) $
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