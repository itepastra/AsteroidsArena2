module Bullet where
import Physics (PhysicsObject (..), HasPhysics (..), accelerate, move)
import TypeClasses (Pictured (..), V2Math (..))
import Sprites (baseBullet)
import Graphics.Gloss (translate)
import Types1 ( TimeStep, Lifetime )

data Bullet = Bullet {phys :: PhysicsObject, lifeTime :: Lifetime}

instance HasPhysics Bullet where
  getPhysObj (Bullet p _) = p
  setPhysObj po a  = a {phys = po}

instance Pictured Bullet where
  getPicture (Bullet {phys = (PhysObj {position=t}), lifeTime = lt } ) = translate (x t) (y t) (baseBullet lt)

updateLifetime :: TimeStep -> Bullet -> Bullet
updateLifetime ts (Bullet p l) = Bullet p (l - ts)

