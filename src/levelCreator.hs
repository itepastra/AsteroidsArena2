module Main where

import AFunctions (AFunction (..), fromString)
import qualified Constants
import Controller (step)
import Data.Set (delete, empty, insert)
import Graphics.Gloss (Display (InWindow))
import Graphics.Gloss.Interface.IO.Game (Event (..), KeyState (..), Picture, SpecialKey (..), black, playIO)
import Graphics.Gloss.Interface.Pure.Game (Key (..))
import LevelHelperFunctions (Part (..), defaultLvlConfig, setPart)
import Model (GameState (..))
import Select (popSelected, selectFirst, selectNext, selectPrev, smap)
import System.Exit (die)
import System.IO (hFlush, stdout)
import Types1 (Selected (NotSelected, Selected, time))
import View (view)
import Wall (InitWall (InitWall))

main :: IO ()
main = do
  name <- askFor "Enter a level name: "
  playIO
    (InWindow "Asteroids Arena 2" Constants.pageSize (0, 0)) -- Or FullScreen
    black -- Background color
    Constants.fps -- Frames per second
    (CreatorState {elapsedTime = 0, iwalls = [], keys = empty, ilevelConfig = defaultLvlConfig, lname = name}) -- Initial state
    view -- View function
    inputCreator -- Event function
    step -- Step function

inputCreator :: Event -> GameState -> IO GameState
inputCreator (EventKey (SpecialKey KeyEsc) Down _ _) g = die "lol"

inputCreator (EventKey (Char 'i') Down _ _) g = do
  weh <- askFor "Enter an Afunction String for strength: "
  let f = setPart Str (fromString weh)
  pure $ g {iwalls = smap f $ iwalls g}
inputCreator (EventKey (Char 'o') Down _ _) g = do
  weh <- askFor "Enter an Afunction String for Offset: "
  let f = setPart Off (fromString weh)
  pure $ g {iwalls = smap f $ iwalls g}
inputCreator (EventKey (Char 'p') Down _ _) g = do
  weh <- askFor "Enter an Afunction String for Rotation: "
  let f = setPart Rot (fromString weh)
  pure $ g {iwalls = smap f $ iwalls g}

inputCreator k s = ((pure .) . pureInput) k s

pureInput :: Event -> GameState -> GameState
-- wall select (previous / next)
pureInput (EventKey (Char 'e') Down _ _) g = g {iwalls = selectNext $ iwalls g}
pureInput (EventKey (Char 'q') Down _ _) g = g {iwalls = selectPrev $ iwalls g}
-- create a new wall
pureInput (EventKey (Char 'n') Down _ _) g = g {iwalls = selectFirst (Selected 120 newWall : iwalls g)}
-- remove the selected wall
pureInput (EventKey (Char '\b') Down _ _) g = g {iwalls = selectFirst $ popSelected $ iwalls g}
pureInput (EventKey k Down _ _) g@(GameState {}) = g {keys = insert k (keys g)}
pureInput (EventKey k Up _ _) g@(GameState {}) = g {keys = delete k (keys g)}
-- pattern match for completeness
pureInput _ g = g

newWall :: InitWall
newWall = InitWall (C 0) (C 0) (C 0)

askFor :: String -> IO String
askFor s = do
  putStr s
  hFlush stdout
  getLine