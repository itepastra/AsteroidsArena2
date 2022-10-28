module Asteroid where

import AsteroidSpawnFunctions (expRandom)
import qualified Constants
import Data.Fixed (mod')
import Graphics.Gloss (rotate, scale, translate)
import Physics (HasPhysics (..), PhysicsObject (..), accelStep, accelerate, frictionStep, move)
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseAsteroid, baseSpaceMine)
import System.Random (Random (..), RandomGen, StdGen)
import System.Random.Stateful (randomM)
import TypeClasses (Pictured (..), V2Math (..))
import Types1 (TimeStep, Size, Time)
import VectorCalc ( Point(Point) )

data Asteroid
  = Asteroid
      { phys :: PhysicsObject,
        size :: Size,
        rotateSpeed :: Float,
        rotateAngle :: Float
      }
  | SpaceMine
      { phys :: PhysicsObject,
        size :: Size,
        rotateSpeed :: Float,
        rotateAngle :: Float
      }

instance HasPhysics Asteroid where
  getPhysObj = phys
  setPhysObj a po = a {phys = po}

instance Pictured Asteroid where
  getPicture (SpaceMine {phys = (PhysObj {position = t}), size = s, rotateAngle = ra}) = translate (x t) (y t) $ Graphics.Gloss.rotate (-ra) $ scale f f baseSpaceMine
    where
      f = 2 ^ s
  getPicture (Asteroid {phys = (PhysObj {position = t}), size = s, rotateAngle = ra}) = translate (x t) (y t) $ Graphics.Gloss.rotate (-ra) $ scale f f baseAsteroid
    where
      f = 2 ^ s

instance Rotate Asteroid where
  rotate a asteroid = asteroid {rotateAngle = (rotateAngle asteroid + a) `mod'` 360}

genRandomAsteroid :: Time -> StdGen -> PhysicsObject -> (StdGen, Asteroid, Float)
genRandomAsteroid t g0 p = (g, constr (PhysObj pos vel rad) size rSpeed rAngle, timeTillNext)
  where
    ((spawnAngle, moveAngle, size, uTime, moveSpeed, rSpeed, rAngle), g1) = randomR ((0, -25, 1, 0, 20, -15, 0), (360, 25, 3, 1, 80, 15, 360)) g0
    (atype, g) = randomR (0, 1) g1
    timeTillNext = expRandom t uTime
    pos = position p |+| (Constants.spawnDistance |*| rot spawnAngle (Point 1 0))
    vel = (rot moveAngle . (moveSpeed |*|) . normalize) (position p |-| pos)
    rad = Constants.asteroidRadius * (2 ^ size)
    constr
      | atype < Constants.spaceMineOdds = SpaceMine
      | otherwise = Asteroid

getChildAsteroids :: (Angle, Float, Float) -> Asteroid -> [Asteroid]
getChildAsteroids _ (Asteroid {size = 1}) = []
getChildAsteroids (angle, speed, rSpeed) (Asteroid {size = s, phys = phy, rotateAngle = ra, rotateSpeed = rss}) = ca
  where
    ca =
      zipWith
        ( \a rs ->
            Asteroid
              { size = s - 1,
                phys = phy {velocity = (speed |*| rot (angle + a) (Point 1 0)) |+| velocity phy, radius = radius phy / 2},
                rotateSpeed = rs + rss,
                rotateAngle = rs + ra
              }
        )
        [-120, 0, 120]
        [0, -rSpeed, rSpeed]
getChildAsteroids _ (SpaceMine {}) = []

track :: PhysicsObject -> TimeStep -> Asteroid -> Asteroid
track _ _ a@(Asteroid {}) = a
track p secs a@(SpaceMine {}) = frictionStep Constants.asteroidFrictionExponent secs . accelStep secs (((300 ^ 2) / (pp |#| pa)) |*| (pp |-| pa)) $ a
  where
    pp = position p
    pa = (position . getPhysObj) a
