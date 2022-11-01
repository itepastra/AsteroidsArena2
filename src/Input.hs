module Input where

import Controller (stateSelect)
import qualified Data.Maybe
import Data.Set (delete, empty, insert)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Down, Up))
import Model (GameState (..), gameStateFromLevel, newPlayer)
import Select (getSelected, selectNext, selectPrev)
import Types1 (Hud (Visible))

input :: Event -> GameState -> IO GameState
input = (pure .) . pureInput

pureInput :: Event -> GameState -> GameState
pureInput (EventKey (Char 's') Down _ _) g@(MenuState {levels = lvls}) = g {levels = selectNext lvls, selectedState = stateSelect (selectNext lvls) (rand g)}
pureInput (EventKey (Char 'w') Down _ _) g@(MenuState {levels = lvls}) = g {levels = selectPrev lvls, selectedState = stateSelect (selectPrev lvls) (rand g)}
pureInput (EventKey (Char 'e') Down _ _) g@(MenuState {levels = lvls}) = Data.Maybe.fromMaybe g (selectedState g)
pureInput (EventKey (Char 'm') Down _ _) g@(PauseState {}) = (previousState g) {keys = empty}
pureInput (EventKey (Char 'm') Down _ _) g@(GameState {}) = PauseState {previousState = g}
pureInput (EventKey (Char 'r') Down _ _) g@(DeathState {}) = retryState g
pureInput (EventKey k Down _ _) g@(GameState {}) = g {keys = insert k (keys g)}
pureInput (EventKey k Up _ _) g@(GameState {}) = g {keys = delete k (keys g)}
pureInput _ g = g

retryState :: GameState -> GameState
retryState g =
  GameState
    { elapsedTime = 0,
      player = newPlayer,
      asteroids = [],
      rand = rand $ previousState g,
      bullets = [],
      walls = walls $ previousState g,
      timeSinceLastShot = 10, --
      timeTillNextAsteroid = 0,
      score = 0,
      levelConfig = levelConfig $ previousState g,
      keys = empty,
      starPositions = starPositions $ previousState g,
      frameTime = 0,
      hud = Visible
    }