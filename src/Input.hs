module Input where

import Data.Set (delete, insert, empty)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Down, Up))
import Model (GameState (GameState, PauseState, keys, previousState))

input :: Event -> GameState -> IO GameState
input = (pure .) . pureInput

pureInput :: Event -> GameState -> GameState
pureInput (EventKey (Char 'm') Down _ _) g@(PauseState {}) = (previousState g) {keys = empty}
pureInput (EventKey (Char 'm') Down _ _) g@(GameState {}) = PauseState {previousState = g}
pureInput (EventKey k Down _ _) g@(GameState {}) = g {keys = insert k (keys g)}
pureInput (EventKey k Up _ _) g@(GameState {}) = g {keys = delete k (keys g)}
pureInput _ g = g


