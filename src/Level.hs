module Level where

import AsteroidSpawnFunctions (DecayFunctions, RandomFunctions)
import Types1 (IntervalTime, Time, TimeAvg, UniformTime)

data InitLevelConfig = InitLevelConfig
  { iasteroidSpawnFunction :: RandomFunctions,
    iasteroidDecayFunction :: DecayFunctions
  }

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: TimeAvg -> UniformTime -> IntervalTime,
    asteroidDecayFunction :: IntervalTime -> Time -> TimeAvg
  }