module Level where
import Types1 (Time, UniformTime)

data LevelConfig = LevelConfig
  { asteroidSpawnFunction :: String
  }


aaa :: (b -> a -> c) -> (a -> b) -> a -> c
aaa = (=<<)

