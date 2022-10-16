{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Asteroid (Asteroid (Asteroid))
import Bullet (Bullet)
import Constants (asteroidRadius, playerRadius)
import Data.Set (Set, empty)
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game (Key)
import Physics (PhysicsObject (..))
import Player (Lives, Player (Player))
import System.Random (RandomGen, StdGen)
import System.Random.Stateful (mkStdGen)
import VectorCalc (Point (Point))
import Wall (Wall (Wall))

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
        keys :: Set Key,
        rand :: StdGen,
        starPositions :: [Gloss.Point],
        timeSinceLastShot :: Float,
        timeTillNextAsteroid :: Float,
        score :: Int
      }
  | DeathState {lives :: Lives}
  | MenuState {levels :: [Level]}

newPlayer :: Player
newPlayer = Player (PhysObj (Point 0 0) (Point 0 0) playerRadius) 0 (Point 0 1) 0

gameStateFromLevel :: StdGen -> [Gloss.Point] -> Level -> GameState
gameStateFromLevel r pts (Level {startState = gs}) = gs {rand = r, starPositions = pts}

defaultLevels :: [Level]
defaultLevels =
  [ Level
      { name = "empty",
        startState =
          GameState
            { elapsedTime = 0,
              player = newPlayer,
              asteroids = [],
              bullets = [],
              walls = [Wall (Point 0 400) (Point 0 (-1)) 450 180, Wall (Point (-400) 0) (Point 1 0) 450 (-90), Wall (Point 0 (-400)) (Point 0 1) 450 0, Wall (Point 400 0) (Point (-1) 0) 450 90],
              keys = empty,
              rand = mkStdGen 0,
              starPositions = [],
              timeSinceLastShot = 0,
              timeTillNextAsteroid = 0,
              score = 0
            }
      }
  ]