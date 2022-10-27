module AsteroidSpawnFunctions where

import qualified Constants
import Types1 (UniformTime, Time)


expRandom :: UniformTime -> Time
expRandom uTime = -Constants.asteroidSpawnAverageInterval * log uTime

uniRandom :: UniformTime -> Time
uniRandom uTime = Constants.asteroidSpawnAverageInterval * 2 * uTime