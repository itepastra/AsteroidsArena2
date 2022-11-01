module Stars where
import System.Random (StdGen, Random (randomRs), RandomGen (split))
import qualified Data.Bifunctor
import qualified Constants
import Data.List (iterate')


genStarPositions :: StdGen -> [Int] -> [[(Float, Float)]]
genStarPositions ra amts =
  zipWith
    (\amt r -> take amt $ randomRs ((0, 0), Data.Bifunctor.bimap fromIntegral fromIntegral Constants.pageSize) r)
    amts
    (iterate' (snd . split) ra)
