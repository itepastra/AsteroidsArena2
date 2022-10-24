module Main where

import qualified Constants
import Controller
import qualified Data.Bifunctor
import Data.List (iterate')
import Graphics.Gloss (Display (InWindow), black, play)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Model
import System.Random (Random (randomRs), RandomGen (split), StdGen, getStdGen)
import qualified VectorCalc
import View

main :: IO ()
main =
  do
    randGen <- getStdGen
    let sps = genStarPositions randGen Constants.starAmount
    playIO
      (InWindow "Asteroids Arena 2" Constants.pageSize (0, 0)) -- Or FullScreen
      black -- Background color
      Constants.fps -- Frames per second
      (gameStateFromLevel randGen sps $ head defaultLevels) -- Initial state
      view -- View function
      input -- Event function
      step -- Step function

genStarPositions :: StdGen -> [Int] -> [[(Float, Float)]]
genStarPositions ra amts =
  zipWith
    (\amt r -> take amt $ randomRs ((0, 0), Data.Bifunctor.bimap fromIntegral fromIntegral Constants.pageSize) r)
    amts
    (iterate' (snd . split) ra)
