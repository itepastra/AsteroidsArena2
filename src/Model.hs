{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Asteroid (Asteroid (Asteroid))
import AsteroidSpawnFunctions (DecayFunctions (..), RandomFunctions (..), expDecay, expRandom, getDecayFunc, getRandomFunc)
import Bullet (Bullet)
import Constants (asteroidRadius, playerMaxHp, playerRadius)
import Data.Set (Set, empty)
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game (Key)
import Level (InitLevelConfig (..), LevelConfig (LevelConfig))
import Physics (PhysicsObject (..))
import Player (Player (Player))
import System.Random (RandomGen, StdGen)
import System.Random.Stateful (mkStdGen)
import Types1 (Time, TimeStep)
import VectorCalc (Point (Point))
import Wall (Wall (Wall))

data GameState
  = GameState
      { elapsedTime :: Float,
        player :: Player,
        asteroids :: [Asteroid],
        bullets :: [Bullet],
        walls :: [Wall],
        keys :: Set Key,
        rand :: StdGen,
        starPositions :: [[Gloss.Point]],
        timeSinceLastShot :: Float,
        timeTillNextAsteroid :: Float,
        score :: Int,
        levelConfig :: LevelConfig,
        frameTime :: TimeStep
      }
  | DeathState
      { previousState :: GameState,
        timeSinceDeath :: Time
      }
  | MenuState
      { levels :: [Level]
      }
  | PauseState
      { previousState :: GameState
      }

data Level = Level
  { name :: String,
    initState :: GameStateInit
  }

data GameStateInit = GameStateInit
  { initWalls :: [Wall],
    initConf :: InitLevelConfig
  }

newPlayer :: Player
newPlayer = Player (PhysObj (Point 0 0) (Point 0 0) playerRadius) Constants.playerMaxHp (Point 0 1) 0

gameStateFromLevel :: StdGen -> [[Gloss.Point]] -> Level -> GameState
gameStateFromLevel r pts (Level {initState = initState}) =
  GameState
    { rand = r,
      starPositions = pts,
      elapsedTime = 0,
      player = newPlayer,
      asteroids = [],
      bullets = [],
      timeSinceLastShot = 10,
      timeTillNextAsteroid = 0,
      score = 0,
      walls = initWalls initState,
      keys = empty,
      levelConfig = initToReal (initConf initState),
      frameTime = 0
    }

defaultLevels :: [Level]
defaultLevels =
  [ Level
      { name = "empty",
        initState =
          GameStateInit
            { initWalls =
                [ Wall (Point 0 400) (Point 0 (-1)) 450 180,
                  Wall (Point (-400) 0) (Point 1 0) 450 (-90),
                  Wall (Point 0 (-400)) (Point 0 1) 450 0,
                  Wall (Point 400 0) (Point (-1) 0) 450 90
                ],
              initConf = emptyLvlConf
            }
      }
  ]

emptyLvlConf :: InitLevelConfig
emptyLvlConf = InitLevelConfig ExpRandom ExpDecay

initToReal :: InitLevelConfig -> LevelConfig
initToReal ic = LevelConfig (getRandomFunc (iasteroidSpawnFunction ic)) (getDecayFunc (iasteroidDecayFunction ic))