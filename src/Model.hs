{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where
import Vector (Point, Vector)

data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1


type Position = Point Double

type Normal   = Vector Double
type Velocity = Vector Double

data Player = Player
  { lives :: Int,
    position :: Position,
    velocity :: Velocity
  }

data Asteroid = Asteroid
  { size :: Int,
    aposition :: (Double, Double),
    avelocity :: (Double, Double)
  }

data Bullet = Bullet
  { lifetime :: Float,
    bposition :: (Double, Double),
    bvelocity :: (Double, Double)
  }

data Wall = Wall {
  base :: Point Double,
  normal :: Vector Double,
  wvelocity :: Velocity
}
  


data GameState = GameState
  { infoToShow :: InfoToShow,
    elapsedTime :: Float
  }

initialState :: GameState
initialState = GameState ShowNothing 0

