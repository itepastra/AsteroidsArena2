module Main where

import AFunctions (AFunction (..), fromString, toString)
import qualified Constants
import Data.Set (delete, empty, insert)
import EditorModel (EditorState (..), levelFromCreatorState)
import Graphics.Gloss (Display (InWindow), Picture (Pictures), rotate, translate)
import Graphics.Gloss.Interface.IO.Game (Event (..), KeyState (..), Modifiers (Modifiers), Picture, SpecialKey (..), black, playIO)
import Graphics.Gloss.Interface.Pure.Game (Key (..))
import Level (Level (name))
import LevelHelperFunctions (Part (..), defaultLvlConfig, modPart, setPart)
import LevelImport (encodeLevel)
import Pictured ()
import Rotation (Rotate (getAngle))
import Select (getAllSelected, popSelected, selectFirst, selectNext, selectPrev, smap)
import Sprites (baseWall, selectedWall)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import TypeClasses (Pictured (..), V2Math (..))
import Types1 (ElapsedTime, Selected (NotSelected, Selected, time))
import Wall (InitWall (..), createWall, point, selfMove)

main :: IO ()
main = do
  name <- askFor "Enter a level name: "
  playIO
    (InWindow "Asteroids Arena 2" Constants.pageSize (0, 0)) -- Or FullScreen
    black -- Background color
    Constants.fps -- Frames per second
    (CreatorState {elapsedTime = 0, iwalls = [], keys = empty, ilevelConfig = defaultLvlConfig, lname = name, timeMultiplier = 0}) -- Initial state
    view -- View function
    inputCreator -- Event function
    step -- Step function

inputCreator :: Event -> EditorState -> IO EditorState
inputCreator (EventKey (SpecialKey KeyEsc) Down _ _) g = exitSuccess
inputCreator (EventKey (Char 'i') Down _ _) g = do
  weh <- askFor ("Enter an function String for strength (current functions " ++ concatMap (\x -> "(" ++ (toString . isFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Str g
inputCreator (EventKey (Char 'o') Down _ _) g = do
  weh <- askFor ("Enter an function String for Offset (current functions " ++ concatMap (\x -> "(" ++ (toString . ioFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Off g
inputCreator (EventKey (Char 'p') Down _ _) g = do
  weh <- askFor ("Enter an function String for Rotation (current functions " ++ concatMap (\x -> "(" ++ (toString . irFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Rot g
inputCreator (EventKey (Char '\DC3') Down (Modifiers _ Down _) _) g = do
  encodeLevel "levels/" $ levelFromCreatorState g
  putStrLn ("saved level at: levels/" ++ name (levelFromCreatorState g))
  pure g
inputCreator k s = do
  print k
  ((pure .) . pureInput) k s

pureInput :: Event -> EditorState -> EditorState
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
-- time controls
pureInput (EventKey (Char 'r') Down _ _) g = g {elapsedTime = 0}
pureInput (EventKey (SpecialKey KeySpace) Down _ _) g = g {timeMultiplier = 0}
pureInput (EventKey (SpecialKey KeyUp) Down _ _) g = g {timeMultiplier = timeMultiplier g + 1}
pureInput (EventKey (SpecialKey KeyDown) Down _ _) g = g {timeMultiplier = timeMultiplier g - 1}
-- pattern match for completeness
pureInput _ g = g

step :: Float -> EditorState -> IO EditorState
step secs gstate@(CreatorState {}) = pure $ gstate {elapsedTime = elapsedTime gstate + (secs * timeMultiplier gstate)}

view :: EditorState -> IO Picture
view = pure . getPicture

newWall :: InitWall
newWall = InitWall (C 0) (C 0) (C 450)

askFor :: String -> IO String
askFor s = do
  putStr s
  hFlush stdout
  getLine

updateWall :: Maybe AFunction -> Part -> EditorState -> EditorState
updateWall (Just f) p g = g {iwalls = smap (setPart p f) $ iwalls g}
updateWall Nothing _ g = g