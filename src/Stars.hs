module Stars where

import qualified Constants
import Data.List (iterate')
import System.Random (Random (randomRs), RandomGen (split), StdGen)
import GeneralHelperFunctions (biFloat)

genStarPositions :: StdGen -> [Int] -> [[(Float, Float)]]
genStarPositions ra amts =
  zipWith
    (\amt r -> take amt $ randomRs ((0, 0), biFloat Constants.pageSize) r)
    amts
    (iterate' (snd . split) ra)
