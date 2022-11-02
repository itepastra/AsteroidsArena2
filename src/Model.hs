{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
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
import Level (InitLevelConfig (..), LevelConfig (LevelConfig))
import Physics (PhysicsObject (..))
import Player (Player (Player))
import System.Random (RandomGen, StdGen)
import System.Random.Stateful (mkStdGen)
import Types1 (Hud (Invisible, Visible), Offset, Selected, Strength, Time, TimeStep)
import VectorCalc (Point (Point))
import Wall (Wall (Wall, frameRotation), createWall)

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
        frameTime :: TimeStep,
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

data Level = Level
  { name :: String,
    initState :: GameStateInit
  }

instance Show Level where
  show :: Level -> String
  show f = show [name f, (show . length . initWalls . initState) f]

data GameStateInit = GameStateInit
  { initWalls :: [Wall],
    initConf :: InitLevelConfig
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
      walls = initWalls initState,
      keys = empty,
      levelConfig = initToReal (initConf initState),
      frameTime = 0,
      hud = Invisible
    }

defaultLevels :: [Level]
defaultLevels =
  [ Level
      { name = "1 - empty",
        initState =
          GameStateInit
            { initWalls =
                [],
              initConf = emptyLvlConf
            }
      },
    Level
      { name = "2 - box",
        initState =
          GameStateInit
            { initWalls =
                wallPoly 4 400 450,
              initConf = emptyLvlConf
            }
      },
    Level
      { name = "3 - triangle",
        initState =
          GameStateInit
            { initWalls =
                wallPoly 3 400 450,
              initConf = emptyLvlConf
            }
      },
    Level
      { name = "4 - ManyGon",
        initState =
          GameStateInit
            { initWalls =
                zipWith (\r w -> w {frameRotation = r}) [1 ..] $
                  wallPoly 11 400 450,
              initConf = emptyLvlConf
            }
      },
    Level
      { name = "5 - Twisty Line",
        initState =
          GameStateInit
            { initWalls =
                wallPoly 2 400 450,
              initConf = emptyLvlConf
            }
      }
  ]

emptyLvlConf :: InitLevelConfig
emptyLvlConf = InitLevelConfig ExpRandom ExpDecay Pow Constants.asteroidSpawnAverageInterval

initToReal :: InitLevelConfig -> LevelConfig
initToReal ic = LevelConfig (getRandomFunc (iasteroidSpawnFunction ic) . getDecayFunc (iasteroidDecayFunction ic) (iasteroidSpawnStart ic)) (getSpaceMineOddFunc (ispaceMineOddsFunction ic))

instance Eq Level where
  l1 == l2 = name l1 == name l2

instance Ord Level where
  compare :: Level -> Level -> Ordering
  compare l1 l2 = compare (name l1) (name l2)

wallPoly :: Int -> Offset -> Strength -> [Wall]
wallPoly n o s = map (\x -> createWall o (fromIntegral x * 360 / fromIntegral n) s) [1 .. n]