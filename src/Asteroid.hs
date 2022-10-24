module Asteroid where

import AsteroidSpawnFunctions (expRandom)
import qualified Constants
import Data.Fixed (mod')
import Graphics.Gloss (rotate, scale, translate)
import Physics (HasPhysics (..), PhysicsObject (..), accelerate, move)
import Player (Player)
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseAsteroid)
import System.Random (Random (..), RandomGen, StdGen)
import System.Random.Stateful (randomM)
import TypeClasses (Pictured (..), V2Math (..))
import VectorCalc (Point (Point))

type Size = Int

data Asteroid = Asteroid {phys :: PhysicsObject, size :: Size, rotateSpeed :: Float, rotateAngle :: Float} | SpaceMine {phys :: PhysicsObject, size :: Size, rotateSpeed :: Float, rotateAngle :: Float}

instance HasPhysics Asteroid where
  physobj = phys
  moveStep a dt = a {phys = move (phys a) dt}
  accelStep as dt a = as {phys = accelerate (phys as) dt a}

instance Pictured Asteroid where
  getGlobalPicture (SpaceMine {phys = (PhysObj {position = t}), size = s, rotateAngle = ra}) = translate (x t) (y t) $ Graphics.Gloss.rotate (-ra) $ scale f f baseAsteroid
    where
      f = 2 ^ s
  getGlobalPicture (Asteroid {phys = (PhysObj {position = t}), size = s, rotateAngle = ra}) = translate (x t) (y t) $ Graphics.Gloss.rotate (-ra) $ scale f f baseAsteroid
    where
      f = 2 ^ s

genRandomAsteroid :: StdGen -> Player -> (StdGen, Asteroid, Float)
genRandomAsteroid g0 p = (g, Asteroid (PhysObj pos vel rad) size rSpeed rAngle, timeTillNext)
  where
    ((spawnAngle, moveAngle, size, uTime, moveSpeed, rSpeed, rAngle), g) = randomR ((0, -25, 1, 0, 20, -15, 0), (360, 25, 3, 1, 80, 15, 360)) g0
    timeTillNext = expRandom uTime
    pos = position (physobj p) |+| (Constants.spawnDistance |*| rot spawnAngle (Point 1 0))
    vel = (rot moveAngle . (moveSpeed |*|) . normalize) (position (physobj p) |-| pos)
    rad = Constants.asteroidRadius * (2 ^ size)

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

instance Rotate Asteroid where
  rotate a asteroid = asteroid {rotateAngle = (rotateAngle asteroid + a) `mod'` 360}
