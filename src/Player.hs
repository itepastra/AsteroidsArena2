module Player where

import Bullet (Bullet (Bullet))
import Constants (bulletInitialOffset, bulletLifetime, bulletSpeed, playerAcceleration, playerFrictionExponent)
import Graphics.Gloss (translate)
import qualified Graphics.Gloss as Gloss
import Physics (Acceleration, HasPhysics (..), PhysicsObject (..), TimeStep, accelerate, move)
import Rotation (Angle, Rotate (..), rot)
import Sprites (basePlayer)
import TypeClasses (Pictured (..), V2Math (..))
import VectorCalc (Vector)

type HealthPoints = Float

type LookDirection = Vector

data Player = Player {phys :: PhysicsObject, hp :: HealthPoints, lookDirection :: LookDirection, lookAngle :: Angle}

instance HasPhysics Player where
  physobj = phys
  moveStep p dt = p {phys = move (phys p) dt}
  accelStep p dt a = p {phys = accelerate (phys p) dt a}

instance Rotate Player where
  rotate a p = p {lookDirection = rot a (lookDirection p), lookAngle = lookAngle p - a}

instance Pictured Player where
  getGlobalPicture (Player {phys = (PhysObj {position = t}), lookAngle = a}) = translate (x t) (y t) $ Gloss.rotate a basePlayer

lookAccel :: Player -> Acceleration
lookAccel p = Constants.playerAcceleration |*| lookDirection p

shoot :: Player -> Bullet
shoot (Player {phys = phy, lookDirection = ld}) = Bullet (PhysObj (position phy |+| pv) (velocity phy |+| bv) 20) bulletLifetime
  where
    bv = bulletSpeed |*| ld
    pv = bulletInitialOffset |*| ld

damage :: Player -> Float -> Player
damage p d = p {hp = hp p - d}

isDead :: Player -> Bool
isDead (Player {hp = h}) = h <= 0

friction :: TimeStep -> Player -> Player
friction ts p@(Player phy l d an) = p {phys = (phy {velocity = (playerFrictionExponent ** ts) |*| velocity phy})}