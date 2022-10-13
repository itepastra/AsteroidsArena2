module Player where
import Physics (PhysicsObject (..), HasPhysics (..), accelerate, move)
import VectorCalc (Vector, V2Math (..))
import Bullet (Bullet (Bullet))

type Lives = Int

type LookDirection = Vector

data Player = Player PhysicsObject Lives LookDirection

instance HasPhysics Player where
  physobj (Player p _ _) = p
  moveStep (Player phy l d) dt = Player (move phy dt) l d
  accelStep (Player phy l d) dt a = Player (accelerate phy dt a) l d


shoot :: Player -> Bullet
shoot (Player phys l ld) = Bullet (PhysObj (position phys |+| pv) (velocity phys |+| bv) 0.1) 5
  where bv = 34 |*| ld
        pv = 3 |*| ld