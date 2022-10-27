module Bullet where
import Physics (PhysicsObject (..), HasPhysics (..), accelerate, move, TimeStep)
import TypeClasses (Pictured (..), V2Math (..))
import Sprites (baseBullet)
import Graphics.Gloss (translate)

type Lifetime = Float

data Bullet = Bullet {phys :: PhysicsObject, lifeTime :: Lifetime}

instance HasPhysics Bullet where
  getPhysObj (Bullet p _) = p
  setPhysObj a po = a {phys = po}

instance Pictured Bullet where
  getPicture (Bullet {phys = (PhysObj {position=t}), lifeTime = lt } ) = translate (x t) (y t) (baseBullet lt)

updateLifetime :: TimeStep -> Bullet -> Bullet
updateLifetime ts (Bullet p l) = Bullet p (l - ts)

