module Input where

import Controller (stateSelect)
import qualified Data.Maybe
import Data.Set (delete, empty, insert)
import ExitStrings (getRandomString)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char, SpecialKey), KeyState (Down, Up), SpecialKey (KeyEnter, KeyEsc, KeyTab))
import Level (GameStateInit (initConf), InitLevelConfig (iasteroidSpawnFunction), Level (initState))
import Model (GameState (..), gameStateFromLevel)
import Player (newPlayer)
import Select (getSingleSelected, selectNext, selectPrev)
import System.Exit (die)
import System.Random (getStdGen)
import Types1 (Hud (..))

input :: Event -> GameState -> IO GameState
input (EventKey (SpecialKey KeyEsc) Down _ _) g@(MenuState {}) = die =<< getRandomString
input (EventKey (SpecialKey KeyEsc) Down _ _) _ = menuState
input e@(EventKey (SpecialKey KeyEnter) Down _ _) g@(MenuState {levels = lvls}) = do
  print printable
  pure $ pureInput e g
  where
    printable = Just . iasteroidSpawnFunction . initConf . initState =<< getSingleSelected (levels g)
input k s = ((pure .) . pureInput) k s

pureInput :: Event -> GameState -> GameState
-- level select (previous / next)
pureInput (EventKey (Char 's') Down _ _) g@(MenuState {levels = lvls}) = g {levels = selectNext lvls, selectedState = stateSelect (selectNext lvls) (rand g)}
pureInput (EventKey (Char 'w') Down _ _) g@(MenuState {levels = lvls}) = g {levels = selectPrev lvls, selectedState = stateSelect (selectPrev lvls) (rand g)}
-- start playing the level
pureInput (EventKey (SpecialKey KeyEnter) Down _ _) g@(MenuState {levels = lvls}) = (Data.Maybe.fromMaybe g (selectedState g)) {hud = Visible, elapsedTime = 0}
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

menuState :: IO GameState
menuState = do
  randGen <- getStdGen
  pure $ MenuState [] randGen Nothing

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
      hud = Visible
    }
