module AsteroidSpawnFunctions where

import qualified Constants
import Types1 (IntervalTime, Time, TimeAvg, UniformTime)

expRandom :: TimeAvg -> UniformTime -> IntervalTime
expRandom t uTime = (-t) * log uTime

uniRandom :: TimeAvg -> UniformTime -> IntervalTime
uniRandom = (*) . (2 *)

expDecay :: IntervalTime -> Time -> TimeAvg
expDecay i = (i *) . (0.99 **)

divDecay :: IntervalTime -> Time -> TimeAvg
divDecay i = (i /) . (1 +)