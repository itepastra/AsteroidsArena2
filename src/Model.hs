{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Asteroid (Asteroid (Asteroid))
import AsteroidSpawnFunctions (DecayFunctions (..), MapFunctions (Pow), RandomFunctions (..), getDecayFunc, getRandomFunc, getSpaceMineOddFunc)
import Bullet (Bullet)
import Constants (asteroidRadius, asteroidSpawnAverageInterval, playerMaxHp, playerRadius)
import Data.Set (Set, empty)
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game (Key)
import Level (GameStateInit (..), InitLevelConfig (..), Level (..), LevelConfig (LevelConfig), initToReal)
import System.Random (RandomGen, StdGen)
import System.Random.Stateful (mkStdGen)
import Wall (Wall, createWall)
import Player ( Player, Player(..) )
import Types1
    ( Hud(..),
      Time,
      Selected,
      Point(Point),
      Score,
      ElapsedTime,
      PhysicsObject(..) )

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
        timeTillNextAsteroid :: ElapsedTime,
        score :: Score,
        levelConfig :: LevelConfig,
        hud :: Hud
      }
  | DeathState
      { previousState :: GameState,
        timeSinceDeath :: Time
      }
  | MenuState
      { levels :: [Selected Level],
        rand :: StdGen,
        selectedState :: Maybe GameState
      }
  | PauseState
      { previousState :: GameState
      }

newPlayer :: Player
newPlayer = Player (PhysObj (Point 0 0) (Point 0 0) playerRadius) Constants.playerMaxHp (Point 0 1) 0

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
