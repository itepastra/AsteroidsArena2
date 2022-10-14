module Player where

import Bullet (Bullet (Bullet))
import Physics (HasPhysics (..), PhysicsObject (..), TimeStep, accelerate, move, Acceleration)
import Rotation (Angle, Rotate (..), rot)
import VectorCalc (Vector)
import Constants (bulletSpeed, bulletInitialOffset, bulletLifetime, playerAcceleration)
import TypeClasses (Pictured (..), V2Math (..))
import Graphics.Gloss (translate)
import Sprites (basePlayer)
import qualified Graphics.Gloss as Gloss

type Lives = Int

type LookDirection = Vector

data Player = Player PhysicsObject Lives LookDirection Angle

instance HasPhysics Player where
  physobj (Player p _ _ _) = p
  moveStep (Player phy l d a) dt = Player (move phy dt) l d a
  accelStep (Player phy l d an) dt a = Player (accelerate phy dt a) l d an

instance Rotate Player where
  rotate a (Player phys l d an) = Player phys l (rot a d) (an - a)

instance Pictured Player where
  getGlobalPicture (Player (PhysObj {position=t}) _ _ a) = translate (x t) (y t) $ Gloss.rotate a basePlayer


lookAccel :: TimeStep -> Player -> Acceleration
lookAccel dt p@(Player _ _ d _) = (playerAcceleration * dt) |*| d

shoot :: Player -> Bullet
shoot (Player phys _ ld _) = Bullet (PhysObj (position phys |+| pv) (velocity phys |+| bv) 20) bulletLifetime
  where
    bv = bulletSpeed |*| ld
    pv = bulletInitialOffset |*| ld