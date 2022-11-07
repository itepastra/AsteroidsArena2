module Bullet where
  
import TypeClasses (Pictured (..), V2Math (..), HasPhysics (..))
import Sprites (baseBullet)
import Graphics.Gloss (translate)
import Types1 (Lifetime, TimeStep)
import Physics (PhysicsObject(..))

data Bullet = Bullet
  { phys :: PhysicsObject,
    lifeTime :: Lifetime
  }
instance HasPhysics Bullet where
  getPhysObj (Bullet p _) = p
  setPhysObj po a  = a {phys = po}

updateLifetime :: TimeStep -> Bullet -> Bullet
updateLifetime ts (Bullet p l) = Bullet p (l - ts)

