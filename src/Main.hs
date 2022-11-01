module Main where

import qualified Constants
import Controller (step)
import Data.Aeson (encodeFile)
import qualified Data.Bifunctor
import Data.List (iterate')
import Graphics.Gloss (Display (InWindow), black, play)
import Graphics.Gloss.Interface.IO.Game (playIO)
import Input (input)
import LevelImport (cleanFileLevels, encodeLevels, fileLevels)
import Model (GameState (MenuState), Level (name), defaultLevels, gameStateFromLevel)
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree, flattenDir, readDirectory)
import System.Random (Random (randomRs), RandomGen (split), StdGen, getStdGen)
import qualified VectorCalc
import View (view)

main :: IO ()
main =
  do
    encodeLevels defaultLevels
    randGen <- getStdGen
    -- let sps = genStarPositions randGen Constants.starAmount
    playIO
      (InWindow "Asteroids Arena 2" Constants.pageSize (0, 0)) -- Or FullScreen
      black -- Background color
      Constants.fps -- Frames per second
      (MenuState [] randGen Nothing) -- Initial state
      view -- View function
      input -- Event function
      step -- Step function
