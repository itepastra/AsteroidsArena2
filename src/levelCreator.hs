module Main where

import AFunctions (AFunction (..), fromString, toString)
import qualified Constants
import Controller (step)
import Data.Set (delete, empty, insert)
import Graphics.Gloss (Display (InWindow))
import Graphics.Gloss.Interface.IO.Game (Event (..), KeyState (..), Modifiers (Modifiers), Picture, SpecialKey (..), black, playIO)
import Graphics.Gloss.Interface.Pure.Game (Key (..))
import Level (Level (name))
import LevelHelperFunctions (Part (..), defaultLvlConfig, modPart, setPart)
import LevelImport (encodeLevel)
import Model (GameState (..), levelFromCreatorState)
import Select (getAllSelected, popSelected, selectFirst, selectNext, selectPrev, smap)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Types1 (Selected (NotSelected, Selected, time))
import View (view)
import Wall (InitWall (..))

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
inputCreator (EventKey (SpecialKey KeyEsc) Down _ _) g = exitSuccess
inputCreator (EventKey (Char 'i') Down _ _) g = do
  weh <- askFor ("Enter an Afunction String for strength (current functions " ++ concatMap (\x -> "(" ++ (toString . isFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Str g
inputCreator (EventKey (Char 'o') Down _ _) g = do
  weh <- askFor ("Enter an Afunction String for Offset (current functions " ++ concatMap (\x -> "(" ++ (toString . ioFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Off g
inputCreator (EventKey (Char 'p') Down _ _) g = do
  weh <- askFor ("Enter an Afunction String for Rotation (current functions " ++ concatMap (\x -> "(" ++ (toString . irFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Rot g
inputCreator (EventKey (Char '\DC3') Down (Modifiers _ Down _) _) g =
  case levelFromCreatorState g of
    Nothing -> pure g
    Just le -> do
      encodeLevel "levels/" le
      putStrLn ("saved level at: levels/" ++ name le)
      pure g
inputCreator k s = do
  ((pure .) . pureInput) k s

pureInput :: Event -> GameState -> GameState
-- wall select (previous / next)
pureInput (EventKey (Char 'e') Down _ _) g = g {iwalls = selectNext $ iwalls g}
pureInput (EventKey (Char 'q') Down _ _) g = g {iwalls = selectPrev $ iwalls g}
-- rotate and set offset
pureInput (EventKey (Char 'a') Down _ _) g = g {iwalls = smap (modPart Rot (AddF (C 15))) $ iwalls g}
pureInput (EventKey (Char 'd') Down _ _) g = g {iwalls = smap (modPart Rot (AddF (C (-15)))) $ iwalls g}
pureInput (EventKey (Char 'w') Down _ _) g = g {iwalls = smap (modPart Off (AddF (C (-15)))) $ iwalls g}
pureInput (EventKey (Char 's') Down _ _) g = g {iwalls = smap (modPart Off (AddF (C 15))) $ iwalls g}
-- create a new wall
pureInput (EventKey (Char 'n') Down _ _) g = g {iwalls = selectFirst (Selected 120 newWall : iwalls g)}
-- remove the selected wall
pureInput (EventKey (Char '\b') Down _ _) g = g {iwalls = selectFirst $ popSelected $ iwalls g}
-- pattern match for completeness
pureInput _ g = g

newWall :: InitWall
newWall = InitWall (C 0) (C 0) (C 0)

askFor :: String -> IO String
askFor s = do
  putStr s
  hFlush stdout
  getLine

updateWall :: Maybe AFunction -> Part -> GameState -> GameState
updateWall (Just f) p g = g {iwalls = smap (setPart p f) $ iwalls g}
updateWall Nothing _ g = g