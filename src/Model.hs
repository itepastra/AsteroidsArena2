{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Asteroid (Asteroid)
import Bullet (Bullet)
import Data.Set (Set, empty)
import Graphics.Gloss.Interface.IO.Game (Key)
import Physics (PhysicsObject (..))
import Player (Lives, Player (Player))
import VectorCalc (V2Math (fromTuple))
import Wall (Wall)

data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data Level = Level {name :: String, startState :: GameState}

data GameState
  = GameState
      { elapsedTime :: Float,
        player :: Player,
        asteroids :: [Asteroid],
        bullets :: [Bullet],
        walls :: [Wall],
        keys :: Set Key
      }
  | DeathState {lives :: Lives}
  | MenuState {levels :: [Level]}

newPlayer :: Player
newPlayer = Player (PhysObj (fromTuple (0, 0)) (fromTuple (0, 0)) 50) 0 (fromTuple (0, 1)) 0

defaultLevels :: [Level]
defaultLevels = [Level {name = "empty", startState = GameState 0 newPlayer [] [] [] empty}]