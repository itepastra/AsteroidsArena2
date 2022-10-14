module Main where

import Constants (fps, pageSize, starAmount)
import Controller
import Graphics.Gloss ( black, Display(InWindow), play )
import Model
import System.Random (getStdGen, StdGen, Random (randomRs))
import View
import qualified VectorCalc

main :: IO ()
main =
  do
    randGen <- getStdGen
    sps <- genStarPositions randGen starAmount
    play
      (InWindow "Asteroids Arena 2" pageSize (0, 0)) -- Or FullScreen
      black -- Background color
      fps -- Frames per second
      (gameStateFromLevel randGen sps $ head defaultLevels ) -- Initial state
      view -- View function
      input -- Event function
      step -- Step function

genStarPositions :: StdGen -> Int -> IO [(Float, Float)]
genStarPositions ra amt = return $ take amt $ randomRs ((0,0), (900, 900)) ra 
