{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Constants
import Controller (step)
import Data.Foldable (Foldable (foldl'))
import DefaultLevels (defaultLevels)
import FISQ (fisqrt)
import Graphics.Gloss.Interface.IO.Game (Display (InWindow), black, playIO)
import Input (input)
import LevelImport (encodeLevels)
import Model (GameState (MenuState))
import System.Random (getStdGen)
import System.TimeIt (timeIt, timeItShow, timeItT)
import View (view)

main :: IO ()
main =
  do
    timeIt waa
    timeIt waa2
    encodeLevels defaultLevels
    randGen <- getStdGen
    playIO
      (InWindow "Asteroids Arena 2" Constants.pageSize (0, 0)) -- Or FullScreen
      black -- Background color
      Constants.fps -- Frames per second
      (MenuState [] randGen Nothing) -- Initial state
      view -- View function
      input -- Event function
      step -- Step function

waa :: IO Float
waa = do
  let !a = foldl' (\b a -> fisqrt a) 0 [1 .. 100000000]
  return a

waa2 :: IO Float
waa2 = do
  let !a = foldl' (\b a -> sqrt a) 0 [1 .. 100000000]
  return a