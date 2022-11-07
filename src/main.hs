module Main where

import DefaultLevels (defaultLevels)
import qualified Constants
import LevelImport (encodeLevels)
import System.Random (getStdGen)
import Graphics.Gloss.Interface.IO.Game (playIO, Display (InWindow), black)
import Model (GameState(MenuState))
import View (view)
import Input (input)
import Controller (step)

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
