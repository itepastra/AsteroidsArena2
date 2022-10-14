module Bullet where
import Physics (PhysicsObject, HasPhysics (..), accelerate, move, TimeStep)

type Lifetime = Float

data Bullet = Bullet PhysicsObject Lifetime

instance HasPhysics Bullet where
  physobj (Bullet p _) = p
  moveStep (Bullet phy l) dt = Bullet (move phy dt) l
  accelStep (Bullet phy l) dt a = Bullet (accelerate phy dt a) l

updateLifetime :: TimeStep -> Bullet -> Bullet
updateLifetime ts (Bullet p l) = Bullet p (l - ts)