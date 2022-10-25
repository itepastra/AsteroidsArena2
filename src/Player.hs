
module Player where

import Bullet (Bullet (Bullet))
import qualified Constants
import Data.Aeson (ToJSON (toEncoding), object, (.=), FromJSON (parseJSON), withObject, (.:))
import Graphics.Gloss (Picture (Pictures), translate)
import qualified Graphics.Gloss as Gloss
import Physics (Acceleration, HasPhysics (..), PhysicsObject (..), TimeStep, accelerate, move)
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseExhaust, basePlayer)
import TypeClasses (Pictured (..), V2Math (..))
import VectorCalc (Vector)

type HealthPoints = Float

type LookDirection = Vector

data Player = Player {phys :: PhysicsObject, hp :: HealthPoints, lookDirection :: LookDirection, lookAngle :: Angle}

instance HasPhysics Player where
  getPhysObj = phys
  setPhysObj a po = a {phys = po}

instance Rotate Player where
  rotate a p = p {lookDirection = rot a (lookDirection p), lookAngle = lookAngle p - a}

instance Pictured Player where
  getGlobalPicture (Player {phys = (PhysObj {position = t, velocity = v}), lookAngle = a}) = translate (x t) (y t) $ Pictures $ map (Gloss.rotate a) [baseExhaust v, basePlayer]

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

