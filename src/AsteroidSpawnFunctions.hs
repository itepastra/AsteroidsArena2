module AsteroidSpawnFunctions where

import qualified Constants
import Types1 (IntervalTime, Time, UniformTime, TimeAvg)

expRandom :: TimeAvg -> UniformTime -> IntervalTime
expRandom t uTime = (-t) * log uTime

uniRandom :: TimeAvg -> UniformTime -> IntervalTime
uniRandom = (*) . (2 *)

expDecay :: Time -> TimeAvg
expDecay = (Constants.asteroidSpawnAverageInterval *) . (0.99 **)

divDecay :: Time -> TimeAvg
divDecay = (Constants.asteroidSpawnAverageInterval /) . (1+)