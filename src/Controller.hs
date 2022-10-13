--   This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import System.Random ()
import Player (Player(..))
import Rotation (rotate)
import Graphics.Gloss.Interface.IO.Game (KeyState(..), Event(..),Key(..))
import Data.Set (Set)

-- | Handle one iteration of the game
step :: Float -> GameState -> GameState
step secs gstate
  = -- Just update the elapsed time
     gstate 


input :: Event -> GameState -> GameState
input (EventKey k Down _ _) g = g {player = rotate (-5) (player g) }
input (EventKey k Up _ _) g = g {player = rotate (-5) (player g) }
input _ g = g