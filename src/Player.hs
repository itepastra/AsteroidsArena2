module Player where

import Bullet (Bullet (Bullet))
import Graphics.Gloss (translate, Picture (Pictures))
import qualified Graphics.Gloss as Gloss
import Physics (Acceleration, HasPhysics (..), PhysicsObject (..), TimeStep, accelerate, move)
import Rotation (Angle, Rotate (..), rot)
import Sprites (basePlayer, baseExhaust)
import TypeClasses (Pictured (..), V2Math (..))
import VectorCalc (Vector)
import qualified Constants

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
  getGlobalPicture (Player {phys = (PhysObj {position = t, velocity = v}), lookAngle = a}) = translate (x t) (y t) $ Pictures $ map (Gloss.rotate a) [baseExhaust v ,basePlayer]

lookAccel :: Player -> Acceleration
lookAccel p = Constants.playerAcceleration |*| lookDirection p

shoot :: Player -> Bullet
shoot (Player {phys = phy, lookDirection = ld}) = Bullet (PhysObj (position phy |+| pv) (velocity phy |+| bv) Constants.bulletRadius) Constants.bulletLifetime
  where
    bv = Constants.bulletSpeed |*| ld
    pv = Constants.bulletInitialOffset |*| ld

damage :: Player -> Float -> Player
damage p d = p {hp = hp p - d}

isDead :: Player -> Bool
isDead (Player {hp = h}) = h <= 0

friction :: TimeStep -> Player -> Player
friction ts p@(Player phy l d an) = p {phys = (phy {velocity = (Constants.playerFrictionExponent ** ts) |*| velocity phy})}

