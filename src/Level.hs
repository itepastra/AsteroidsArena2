module Level where

import AsteroidSpawnFunctions ()
import Data.Map (fromList)
import Types1
  ( ElapsedTime,
    IntervalTime,
    Time,
    UniformTime,
    Var (..),
  )
import VFunctions (VFunction, mkNumFunc)
import Wall (InitWall)

data Level = Level
  { name :: String,
    initState :: GameStateInit
  }
  deriving (Show)

data GameStateInit = GameStateInit
  { initWalls :: [InitWall],
    initConf :: InitLevelConfig
  }
  deriving (Show)

data InitLevelConfig = InitLevelConfig
  { -- iasteroidSpawnFunction should use Y for elapsed time and X for a random number [0,1), and Z for a starting interval multiplier (Z gets filled in on creation)
    iasteroidSpawnFunction :: VFunction Float Var,
    -- ispaceMineOddsFunction should use X for elapsed time and returns a number [0,1)
    ispaceMineOddsFunction :: VFunction Float Var,
    iasteroidSpawnStart :: Time
  }
  deriving (Show)

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: ElapsedTime -> UniformTime -> IntervalTime,
    spaceMineOddsFunction :: ElapsedTime -> Float
  }

initToReal :: InitLevelConfig -> LevelConfig
initToReal ic = LevelConfig af sf
  where
    af et ut = mkNumFunc (iasteroidSpawnFunction ic) (fromList [(Z, iasteroidSpawnStart ic), (Y, et), (X, ut)])
    sf et = mkNumFunc (ispaceMineOddsFunction ic) (fromList [(X, et)])

instance Eq Level where
  l1 == l2 = name l1 == name l2

instance Ord Level where
  compare l1 l2 = compare (name l1) (name l2)