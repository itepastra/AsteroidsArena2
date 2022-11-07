module Main where

import qualified Constants
import Controller (step)
import DefaultLevels (defaultLevels)
import Graphics.Gloss.Interface.IO.Game (Display (InWindow), black, playIO)
import Input (input)
import LevelImport (encodeLevels)
import Model (GameState (MenuState))
import System.Random (getStdGen)
import View (view)

main :: IO ()
main =
  do
    encodeLevels defaultLevels
    randGen <- getStdGen
    playIO
      (InWindow "Asteroids Arena 2" Constants.pageSize (0, 0)) -- Or FullScreen
      black -- Background color
      Constants.fps -- Frames per second
      (MenuState [] randGen Nothing) -- Initial state
      view -- View function
      input -- Event function
      step -- Step function
