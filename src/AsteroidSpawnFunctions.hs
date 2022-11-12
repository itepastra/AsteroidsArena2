module AsteroidSpawnFunctions where

import Data.Map (fromList)
import Types1 (ElapsedTime, IntervalTime, Time, TimeAvg, UniformTime, Var (..))
import VFunctions (VFunction (Variable), mkNumFunc)

-- functions to go from a uniform variable to an different distribution
expRandom :: Floating a => VFunction a Var
expRandom = (-(Variable Z)) * log (Variable X)

uniRandom :: Floating a => VFunction a Var
uniRandom = (Variable Z * 2) * Variable X

-- functions that take a starting value and the total elapsed time to generate a mean time between asteroids

expDecay :: Floating b => VFunction b Var
expDecay = Variable Z * (0.99 ** Variable Y)

divDecay :: Fractional b => VFunction b Var
divDecay = Variable Z / (1 + Variable Y)

-- functions that map an elapsed time to a range between 0 and 1

eSigmoid :: Floating a => VFunction a Var
eSigmoid = recip (1 + Variable X ** Variable Y)

lSigmoid :: Floating a => VFunction a Var
lSigmoid = recip (1 + Variable X * Variable Y)

pow :: Floating a => VFunction a Var
pow = 1 - Variable X ** (-Variable Y)

sin2 :: Floating a => VFunction a Var
sin2 = sin (Variable X * Variable Y) ^ 2
