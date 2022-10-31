{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module AsteroidSpawnFunctions where

import qualified Constants
import Types1 (IntervalTime, Time, TimeAvg, UniformTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

expRandom :: TimeAvg -> UniformTime -> IntervalTime
expRandom t uTime = (-t) * log uTime

uniRandom :: TimeAvg -> UniformTime -> IntervalTime
uniRandom = (*) . (2 *)

expDecay :: IntervalTime -> Time -> TimeAvg
expDecay i = (i *) . (0.99 **)

divDecay :: IntervalTime -> Time -> TimeAvg
divDecay i = (i /) . (1 +)

data RandomFunctions = ExpRandom | UniRandom
    deriving (Generic, ToJSON, FromJSON)
data DecayFunctions = ExpDecay | DivDecay 
    deriving (Generic, ToJSON, FromJSON)

getRandomFunc :: RandomFunctions -> (TimeAvg -> UniformTime -> IntervalTime)
getRandomFunc ExpRandom = expRandom
getRandomFunc UniRandom = uniRandom


getDecayFunc :: DecayFunctions -> (TimeAvg -> UniformTime -> IntervalTime)
getDecayFunc ExpDecay = expRandom
getDecayFunc DivDecay = uniRandom