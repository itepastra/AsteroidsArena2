module Asteroid where

import Physics (HasPhysics (..), PhysicsObject, accelerate, move)

type Size = Int

data Asteroid = Asteroid PhysicsObject Size

instance HasPhysics Asteroid where
  physobj (Asteroid p _) = p
  moveStep (Asteroid phy s) dt = Asteroid (move phy dt) s
  accelStep (Asteroid phy s) dt a = Asteroid (accelerate phy dt a) s
