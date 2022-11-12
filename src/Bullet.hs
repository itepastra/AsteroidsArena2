module Bullet where

import Graphics.Gloss (translate)
import Physics (PhysicsObject (..))
import Sprites (baseBullet)
import TypeClasses (HasPhysics (..), Pictured (..))
import Types1 (Lifetime, TimeStep)

data Bullet = Bullet
  { phys :: PhysicsObject,
    lifeTime :: Lifetime
  }

instance HasPhysics Bullet where
  getPhysObj (Bullet p _) = p
  setPhysObj po a = a {phys = po}

updateLifetime :: TimeStep -> Bullet -> Bullet
updateLifetime ts (Bullet p l) = Bullet p (l - ts)
