module AsteroidSpawnFunctions where

import qualified Constants

type Time = Float

type UniformTime = Time

expRandom :: UniformTime -> Time
expRandom uTime = -Constants.asteroidSpawnAverageInterval * log uTime

uniRandom :: UniformTime -> Time
uniRandom uTime = Constants.asteroidSpawnAverageInterval * 2 * uTime