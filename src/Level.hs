module Level where
import AsteroidSpawnFunctions (UniformTime, Time)

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: UniformTime -> Time,
    wah :: String
  }