module Stars where

import qualified Constants
import qualified Data.Bifunctor
import Data.List (iterate')
import System.Random (Random (randomRs), RandomGen (split), StdGen)

genStarPositions :: StdGen -> [Int] -> [[(Float, Float)]]
genStarPositions ra amts =
  zipWith
    (\amt r -> take amt $ randomRs ((0, 0), Data.Bifunctor.bimap fromIntegral fromIntegral Constants.pageSize) r)
    amts
    (iterate' (snd . split) ra)
