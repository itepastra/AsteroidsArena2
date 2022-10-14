module Main where

import Controller
import Graphics.Gloss
import Model
import View

main :: IO ()
main =
  play
    (InWindow "Counter" (900, 900) (0, 0)) -- Or FullScreen
    black -- Background color
    30 -- Frames per second
    (startState . head $ defaultLevels) -- Initial state
    view -- View function
    input -- Event function
    step -- Step function

