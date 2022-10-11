{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- | This module contains the data types
--   which represent the state of the game
module Model where

import Vector (Point, Vector, Position, Velocity, PhysicsObject (..), pvAdd, vMult, vAdd)

data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1


data Player = Player
  { lives :: Int,
    position :: Position,
    velocity :: Velocity
  }

data Asteroid = Asteroid
  { size :: Int,
    aposition :: Position,
    avelocity :: Velocity
  }

data Bullet = Bullet
  { lifetime :: Float,
    bposition :: Position,
    bvelocity :: Velocity
  }


data GameState = GameState
  { infoToShow :: InfoToShow,
    elapsedTime :: Float
  }

instance PhysicsObject Player where
  timeStep p dt = Player (lives p) (position p `pvAdd` (velocity p `vMult` dt)) (velocity p)
  accelerate p a dt = Player (lives p) (position p) (velocity p `vAdd` (a `vMult` dt))
  collides p o = 
  inWall = _

initialState :: GameState
initialState = GameState ShowNothing 0
