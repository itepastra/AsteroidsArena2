module Bullet where
import Physics (PhysicsObject (..), HasPhysics (..), accelerate, move, TimeStep)
import TypeClasses (Pictured (..), V2Math (..))
import Sprites (baseBullet)
import Graphics.Gloss (translate)

type Lifetime = Float

data Bullet = Bullet {phys :: PhysicsObject, lifeTime :: Lifetime}

instance HasPhysics Bullet where
  physobj (Bullet p _) = p
  moveStep (Bullet phy l) dt = Bullet (move phy dt) l
  accelStep (Bullet phy l) dt a = Bullet (accelerate phy dt a) l

instance Pictured Bullet where
  getGlobalPicture (Bullet {phys = (PhysObj {position=t}), lifeTime = lt } ) = translate (x t) (y t) (baseBullet lt)

updateLifetime :: TimeStep -> Bullet -> Bullet
updateLifetime ts (Bullet p l) = Bullet p (l - ts)