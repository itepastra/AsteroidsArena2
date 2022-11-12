module Bullet where

import Graphics.Gloss (translate)
import Types1 ( PhysicsObject(..), Lifetime, TimeStep )
import Sprites (baseBullet)
import TypeClasses (HasPhysics (..))
import Pictured (Pictured (..), mvWithPhys)

data Bullet = Bullet
  { phys :: PhysicsObject,
    lifeTime :: Lifetime
  }

instance HasPhysics Bullet where
  getPhysObj (Bullet p _) = p
  setPhysObj po a = a {phys = po}

instance Pictured Bullet where
  getPicture o@(Bullet {}) = mvWithPhys o (baseBullet (lifeTime o))


updateLifetime :: TimeStep -> Bullet -> Bullet
updateLifetime ts (Bullet p l) = Bullet p (l - ts)
