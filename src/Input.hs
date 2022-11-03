module Input where

import Controller (stateSelect)
import qualified Data.Maybe
import Data.Set (delete, empty, insert)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), KeyState (Down, Up), SpecialKey (KeyEnter, KeyTab, KeyEsc))
import Model (GameState (..), gameStateFromLevel, newPlayer)
import Select (getSelected, selectNext, selectPrev)
import System.Random (getStdGen)
import Types1 (Hud (Visible))
import System.Exit (die)
import ExitStrings (getRandomString)

input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyEsc) Down _ _) g@(MenuState {} ) = die =<< getRandomString
input (EventKey (SpecialKey KeyEsc) Down _ _) _ = menuState
input k s = ((pure .) . pureInput) k s

menuState :: IO GameState
menuState = do
  randGen <- getStdGen
  pure $ MenuState [] randGen Nothing

pureInput :: Event -> GameState -> GameState
-- level select (previous / next)
pureInput (EventKey (Char 's') Down _ _) g@(MenuState {levels = lvls}) = g {levels = selectNext lvls, selectedState = stateSelect (selectNext lvls) (rand g)}
pureInput (EventKey (Char 'w') Down _ _) g@(MenuState {levels = lvls}) = g {levels = selectPrev lvls, selectedState = stateSelect (selectPrev lvls) (rand g)}

-- start playing the level
pureInput (EventKey (SpecialKey KeyEnter) Down _ _) g@(MenuState {levels = lvls}) = (Data.Maybe.fromMaybe g (selectedState g)) {hud = Visible}

-- toggle pause
pureInput (EventKey (SpecialKey KeyTab) Down _ _) g@(PauseState {}) = (previousState g) {keys = empty}
pureInput (EventKey (SpecialKey KeyTab) Down _ _) g@(GameState {}) = PauseState {previousState = g}

-- retry
pureInput (EventKey (Char 'r') Down _ _) g@(DeathState {}) = retryState g
pureInput (EventKey (Char 'r') Down _ _) g@(PauseState {}) = retryState g

-- keys for playing
pureInput (EventKey k Down _ _) g@(GameState {}) = g {keys = insert k (keys g)}
pureInput (EventKey k Up _ _) g@(GameState {}) = g {keys = delete k (keys g)}

-- pattern match for completeness
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
      timeSinceLastShot = 10,
      timeTillNextAsteroid = 0,
      score = 0,
      levelConfig = levelConfig $ previousState g,
      keys = empty,
      starPositions = starPositions $ previousState g,
      frameTime = 0,
      hud = Visible
    }