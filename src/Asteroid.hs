module Asteroid where

import Physics (HasPhysics (..), PhysicsObject (..), accelerate, move)
import Sprites (baseAsteroid)
import TypeClasses (Pictured (..), V2Math (..))
import Graphics.Gloss (translate)

type Size = Int

data Asteroid = Asteroid PhysicsObject Size

instance HasPhysics Asteroid where
  physobj (Asteroid p _) = p
  moveStep (Asteroid phy s) dt = Asteroid (move phy dt) s
  accelStep (Asteroid phy s) dt a = Asteroid (accelerate phy dt a) s


instance Pictured Asteroid where
  getGlobalPicture (Asteroid (PhysObj {position=t}) _ ) = translate (x t) (y t) baseAsteroid
