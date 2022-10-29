module Level where

import Types1 (Time, TimeAvg, UniformTime, IntervalTime)

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: TimeAvg -> UniformTime -> Time,
    asteroidDecayFunction :: IntervalTime -> Time -> TimeAvg
  }
