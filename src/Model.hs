{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Asteroid (Asteroid (Asteroid))
import Bullet (Bullet)
import Constants (asteroidRadius, asteroidSpawnAverageInterval, playerMaxHp, playerRadius)
import Data.Set (Set, empty)
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game (Key)
import Level (GameStateInit (..), InitLevelConfig (..), Level (..), LevelConfig (LevelConfig), initToReal)
import Player (Player (..), newPlayer)
import System.Random (RandomGen, StdGen)
import System.Random.Stateful (mkStdGen)
import Types1
  ( ElapsedTime,
    Hud (..),
    IntervalTime,
    PhysicsObject (..),
    Score,
    Selected (val),
    Time,
  )
import Wall (InitWall (InitWall), Wall, createWall)

data GameState
  = GameState
      { elapsedTime :: ElapsedTime,
        player :: Player,
        asteroids :: [Asteroid],
        bullets :: [Bullet],
        walls :: [Wall],
        keys :: Set Key,
        rand :: StdGen,
        starPositions :: [[Gloss.Point]],
        timeSinceLastShot :: ElapsedTime,
        timeTillNextAsteroid :: IntervalTime,
        score :: Score,
        levelConfig :: LevelConfig,
        hud :: Hud
      }
  | DeathState
      { previousState :: GameState,
        timeSinceDeath :: ElapsedTime
      }
  | MenuState
      { levels :: [Selected Level],
        rand :: StdGen,
        selectedState :: Maybe GameState
      }
  | PauseState
      { previousState :: GameState
      }
  
instance Show GameState where
  show gstate@(GameState {}) = "time: " ++ show (elapsedTime gstate)
  show gstate = error "No implementation for show on any gamestate other than GameState"
  
  
gameStateFromLevel :: StdGen -> Level -> GameState
gameStateFromLevel r (Level {initState = initState}) =
  GameState
    { rand = r,
      starPositions = [],
      elapsedTime = 0,
      player = newPlayer,
      asteroids = [],
      bullets = [],
      timeSinceLastShot = 10,
      timeTillNextAsteroid = 0,
      score = 0,
      walls = map createWall (initWalls initState),
      keys = empty,
      levelConfig = initToReal (initConf initState),
      hud = Invisible
    }

