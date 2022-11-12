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
import VFunctions (VFunction, insert, mkNumFunc)
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
  { iasteroidSpawnFunction :: VFunction Float Var,
    iasteroidDecayFunction :: VFunction Float Var,
    ispaceMineOddsFunction :: VFunction Float Var,
    iasteroidSpawnStart :: Time
  }
  deriving (Show)

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: ElapsedTime -> UniformTime -> IntervalTime,
    spaceMineOddsFunction :: Float -> ElapsedTime -> Float
  }

initToReal :: InitLevelConfig -> LevelConfig
initToReal ic = LevelConfig af sf
  where
    af et ut = mkNumFunc a (fromList [(Z, iasteroidSpawnStart ic), (Y, et), (X, ut)])
    sf f et = mkNumFunc (ispaceMineOddsFunction ic) (fromList [(Y, f), (X, et)])
    a = insert b (iasteroidSpawnFunction ic)
    b Z = iasteroidDecayFunction ic
    b c = pure c

instance Eq Level where
  l1 == l2 = name l1 == name l2

instance Ord Level where
  compare l1 l2 = compare (name l1) (name l2)