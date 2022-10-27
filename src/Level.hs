module Level where
import AsteroidSpawnFunctions (UniformTime, Time)

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: UniformTime -> Time,
    wah :: String
  }


aaa :: (b -> a -> c) -> (a -> b) -> a -> c
aaa = (=<<)

