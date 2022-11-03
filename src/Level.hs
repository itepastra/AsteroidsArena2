module Level where

import AsteroidSpawnFunctions (DecayFunctions, MapFunctions, RandomFunctions, getRandomFunc, getDecayFunc, getSpaceMineOddFunc)
import Types1 (ElapsedTime, IntervalTime, Time, TimeAvg, UniformTime)
import Wall (InitWall)

data InitLevelConfig = InitLevelConfig
  { iasteroidSpawnFunction :: RandomFunctions,
    iasteroidDecayFunction :: DecayFunctions,
    ispaceMineOddsFunction :: MapFunctions,
    iasteroidSpawnStart :: Time
  }

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: ElapsedTime -> UniformTime -> IntervalTime,
    spaceMineOddsFunction :: Float -> ElapsedTime -> Float
  }

data Level = Level
  { name :: String,
    initState :: GameStateInit
  }

instance Show Level where
  show f = show [name f, (show . length . initWalls . initState) f]

data GameStateInit = GameStateInit
  { initWalls :: [InitWall],
    initConf :: InitLevelConfig
  }

initToReal :: InitLevelConfig -> LevelConfig
initToReal ic = LevelConfig (getRandomFunc (iasteroidSpawnFunction ic) . getDecayFunc (iasteroidDecayFunction ic) (iasteroidSpawnStart ic)) (getSpaceMineOddFunc (ispaceMineOddsFunction ic))

instance Eq Level where
  l1 == l2 = name l1 == name l2

instance Ord Level where
  compare l1 l2 = compare (name l1) (name l2)
