module Level where

import AsteroidSpawnFunctions (DecayFunctions, RandomFunctions, MapFunctions)
import Types1 (IntervalTime, Time, TimeAvg, UniformTime, ElapsedTime)

data InitLevelConfig = InitLevelConfig
  { iasteroidSpawnFunction :: RandomFunctions,
    iasteroidDecayFunction :: DecayFunctions,
    ispaceMineOddsFunction :: MapFunctions,
    iasteroidSpawnStart    :: Time
  }

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: ElapsedTime -> UniformTime -> IntervalTime,
    spaceMineOddsFunction :: Float -> ElapsedTime -> Float
  }
