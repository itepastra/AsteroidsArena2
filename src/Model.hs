{-# LANGUAGE InstanceSigs #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

data InfoToShow
  = ShowNothing
  | ShowANumber Int
  | ShowAChar Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1


data Point = Point Double Double
data Vector = Vector Double Double

type Position = Point
type Collision = Point

type Normal   = Vector
type Velocity = Vector



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
  base :: Point,
  normal :: Vector,
  wvelocity :: Velocity
}
  
class PhysicsObject a where
  move :: a -> Float -> a
  reflect :: a -> Normal -> a
  collision ::PhysicsObject b => a -> b -> Maybe Collision

instance PhysicsObject Player where
  move :: Player -> Float -> Player
  move = _
  reflect :: Player -> Normal -> Player
  reflect = _
  collision :: PhysicsObject b => Player -> b -> Maybe Collision
  collision = _
  
instance PhysicsObject Asteroid where
  move :: Asteroid -> Float -> Asteroid
  move = _
  reflect :: Asteroid -> Normal -> Asteroid
  reflect = _
  collision :: PhysicsObject b => Asteroid -> b -> Maybe Collision
  collision = _

instance PhysicsObject Bullet where
  move :: Bullet -> Float -> Bullet
  move = _
  reflect :: Bullet -> Normal -> Bullet
  reflect = _
  collision :: PhysicsObject b => Bullet -> b -> Maybe Collision
  collision = _

instance PhysicsObject Wall where
  move :: Wall -> Float -> Wall
  move = _
  reflect :: Wall -> Normal -> Wall
  reflect = _
  collision :: PhysicsObject b => Wall -> b -> Maybe Collision
  collision = _

data GameState = GameState
  { infoToShow :: InfoToShow,
    elapsedTime :: Float
  }

initialState :: GameState
initialState = GameState ShowNothing 0