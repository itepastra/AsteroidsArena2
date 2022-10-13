{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}


-- | This module contains the data types
--   which represent the state of the game
module Model where
import Player (Player (Player))
import Asteroid (Asteroid)
import Bullet (Bullet)
import Physics (PhysicsObject(..))
import VectorCalc (V2Math(fromTuple))


data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1



data GameState = GameState
  { 
    elapsedTime :: Float,
    player :: Player,
    asteroids :: [Asteroid],
    bullets :: [Bullet],
    walls :: [Wall]
  }

initialState :: GameState
initialState = GameState 0 newPlayer [] []

newPlayer :: Player
newPlayer = Player (PhysObj (fromTuple (0, 0)) (fromTuple (0,0)) 50) 0 (fromTuple (1, 0))

infoToShow = "aaa"
