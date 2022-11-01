module Player where

import Asteroid (Asteroid (Asteroid), size)
import Bullet (Bullet (Bullet))
import qualified Constants
import Data.Aeson (FromJSON (parseJSON), ToJSON (toEncoding), object, withObject, (.:), (.=))
import Data.List (foldl')
import Graphics.Gloss (Picture (Pictures), translate)
import qualified Graphics.Gloss as Gloss
import Physics (HasPhysics (..), PhysicsObject (..), accelerate, checkCollision, move)
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseExhaust, basePlayer)
import TypeClasses (Pictured (..), V2Math (..))
import Types1 (Acceleration, HealthPoints, LookDirection)

data Player
  = Player
      { phys :: PhysicsObject,
        hp :: HealthPoints,
        lookDirection :: LookDirection,
        lookAngle :: Angle
      }

instance HasPhysics Player where
  getPhysObj = phys
  setPhysObj a po = a {phys = po}

instance Rotate Player where
  rotate a p = p {lookDirection = rot a (lookDirection p), lookAngle = lookAngle p - a}

instance Pictured Player where
  getPicture (Player {phys = (PhysObj {position = t, velocity = v}), lookAngle = a}) = translate (x t) (y t) $ Pictures $ map (Gloss.rotate a) [baseExhaust v, basePlayer]

lookAccel :: Player -> Acceleration
lookAccel p = Constants.playerAcceleration |*| lookDirection p

shoot :: Player -> Bullet
shoot p = Bullet (PhysObj (position phy |+| pv) (velocity phy |+| bv) Constants.bulletRadius) Constants.bulletLifetime
  where
    phy = getPhysObj p
    ld = lookDirection p
    bv = Constants.bulletSpeed |*| ld
    pv = 0.1 |*| (velocity phy |+| bv) 

playerHeal :: HealthPoints -> Player -> Player
playerHeal h p@(Player {}) = p {hp = h}

playerDamage :: [Asteroid] -> [Bullet] -> Player -> Player
playerDamage as bs p@(Player {}) = np
  where
    np = case foldl'
      (bulletDamage p)
      (foldl' (asteroidDamage p) (Just (hp p)) as)
      bs of
      Nothing -> p {hp = 0}
      Just x -> p {hp = x}
    bulletDamage _ Nothing _ = Nothing
    bulletDamage p (Just hp) b = if checkCollision p b then (if hp > 15 then Just (hp - 15) else Nothing) else Just hp
    asteroidDamage _ Nothing _ = Nothing
    asteroidDamage p (Just hp) a = if checkCollision p a then (if hp > ad a then Just (hp - ad a) else Nothing) else Just hp
    ad a = 5 * 2 ^ size a