module Main where

import Arbitrary ()
import qualified Constants
import Data.Set (delete, empty, insert)
import EditorModel (EditorState (..), levelFromCreatorState)
import Graphics.Gloss (Display (InWindow), Picture (Pictures), rotate, translate)
import Graphics.Gloss.Interface.IO.Game (Event (..), KeyState (..), Modifiers (Modifiers), Picture, SpecialKey (..), black, playIO)
import Graphics.Gloss.Interface.Pure.Game (Key (..))
import Level (Level (name))
import LevelHelperFunctions (defaultLvlConfig, modPart, setPart)
import LevelImport (encodeLevel)
import Pictured ()
import Rotation (Rotate (getAngle))
import Select (getAllSelected, popSelected, selectFirst, selectNext, selectPrev, smap)
import Sprites (baseWall, selectedWall)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import TypeClasses ( HasA ((#)), Pictured (..), V2Math (..))
import Types1 (ElapsedTime, Part (..), Selected (NotSelected, Selected, time), X)
import VFunctions (VFunction (Constant), fromString, collapse)
import Wall (InitWall (..), createWall, point, selfMove)

{-
The code here is shitty I know, but it's also not direclty for the project and just a side so I dont really care
-}

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
  weh <- askFor ("Enter an function String for strength (current functions " ++ concatMap (\x -> "(" ++ (show . isFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Str g
inputCreator (EventKey (Char 'o') Down _ _) g = do
  weh <- askFor ("Enter an function String for Offset (current functions " ++ concatMap (\x -> "(" ++ (show . ioFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Off g
inputCreator (EventKey (Char 'p') Down _ _) g = do
  weh <- askFor ("Enter an function String for Rotation (current functions " ++ concatMap (\x -> "(" ++ (show . irFunc) x ++ "), ") (getAllSelected $ iwalls g) ++ ") : ")
  pure $ updateWall (fromString weh) Rot g
inputCreator (EventKey (Char '\DC3') Down (Modifiers _ Down _) _) g = do
  encodeLevel "levels/" $ levelFromCreatorState g
  putStrLn ("saved level at: levels/" ++ name (levelFromCreatorState g))
  pure g
inputCreator (EventKey (Char 'z') Down _ _) g = do
  w <- generate arbitrary
  pure g {iwalls = selectFirst (Selected 120 (tupleCollapse # w) : iwalls g)}
inputCreator k s = ((pure .) . pureInput) k s

tupleCollapse :: (VFunction X Float, VFunction X Float, VFunction X Float) -> (VFunction X Float, VFunction X Float, VFunction X Float)
tupleCollapse (a, b, c) = (collapse a, collapse b, collapse c)

pureInput :: Event -> EditorState -> EditorState
-- wall select (previous / next)
pureInput (EventKey (Char 'e') Down _ _) g = g {iwalls = selectNext $ iwalls g}
pureInput (EventKey (Char 'q') Down _ _) g = g {iwalls = selectPrev $ iwalls g}
-- rotate and set offset
pureInput (EventKey (Char 'a') Down _ _) g = g {iwalls = smap (modPart Rot (15 +)) $ iwalls g}
pureInput (EventKey (Char 'd') Down _ _) g = g {iwalls = smap (modPart Rot ((-15) +)) $ iwalls g}
pureInput (EventKey (Char 'w') Down _ _) g = g {iwalls = smap (modPart Off ((-15) +)) $ iwalls g}
pureInput (EventKey (Char 's') Down _ _) g = g {iwalls = smap (modPart Off (15 +)) $ iwalls g}
-- create a new wall
pureInput (EventKey (Char 'n') Down _ _) g = g {iwalls = selectFirst (Selected 120 newWall : iwalls g)}
pureInput (EventKey (Char 'c') Down _ _) g = g {iwalls = selectFirst (head (iwalls g) : iwalls g)}
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
step secs gstate@(CreatorState {}) = pure $ gstate {elapsedTime = elapsedTime gstate + secs * timeMultiplier gstate}

view :: EditorState -> IO Picture
view = pure . getPicture

newWall :: InitWall
newWall = InitWall 0 0 450

askFor :: String -> IO String
askFor s = do
  putStr s
  hFlush stdout
  getLine

updateWall :: Maybe (VFunction X Float) -> Part -> EditorState -> EditorState
updateWall (Just f) p g = g {iwalls = smap (setPart p f) $ iwalls g}
updateWall Nothing _ g = g