{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module AsteroidSpawnFunctions
  ( getRandomFunc,
    getDecayFunc,
    getSpaceMineOddFunc,
    RandomFunctions (..),
    DecayFunctions (..),
    MapFunctions (..),
  )
where

import qualified Constants
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Types1 (ElapsedTime, IntervalTime, Time, TimeAvg, UniformTime)

-- functions to go from a uniform variable to an different distribution

expRandom :: TimeAvg -> UniformTime -> IntervalTime
expRandom t uTime = (-t) * log uTime

uniRandom :: TimeAvg -> UniformTime -> IntervalTime
uniRandom = (*) . (2 *)

-- functions that take a starting value and the total elapsed time to generate a mean time between asteroids

expDecay :: IntervalTime -> ElapsedTime -> TimeAvg
expDecay i et = i * (0.99 ** et)

divDecay :: IntervalTime -> ElapsedTime -> TimeAvg
divDecay i = (i /) . (1 +)

-- functions that map an elapsed time to a range between 0 and 1

eSigmoid :: Float -> ElapsedTime -> Float
eSigmoid b e = recip (1 + b ** e)

lSigmoid :: Float -> ElapsedTime -> Float
lSigmoid b e = recip (1 + b * e)

pow :: Float -> ElapsedTime -> Float
pow b e = 1 - (b ** (-e))

sin2 :: Float -> ElapsedTime -> Float
sin2 f e = sin (f * e) ^ 2

data RandomFunctions = ExpRandom | UniRandom
  deriving (Generic, ToJSON, FromJSON, Show)

data DecayFunctions = ExpDecay | DivDecay
  deriving (Generic, ToJSON, FromJSON, Show)

data MapFunctions = ESigmoid | LSigmoid | Pow | Sin2
  deriving (Generic, ToJSON, FromJSON, Show)

getRandomFunc :: RandomFunctions -> (TimeAvg -> UniformTime -> IntervalTime)
getRandomFunc ExpRandom = expRandom
getRandomFunc UniRandom = uniRandom

getDecayFunc :: DecayFunctions -> (Time -> UniformTime -> TimeAvg)
getDecayFunc ExpDecay = expDecay
getDecayFunc DivDecay = divDecay

getSpaceMineOddFunc :: MapFunctions -> (Float -> ElapsedTime -> Float)
getSpaceMineOddFunc ESigmoid = eSigmoid
getSpaceMineOddFunc LSigmoid = lSigmoid
getSpaceMineOddFunc Pow = pow
getSpaceMineOddFunc Sin2 = sin2