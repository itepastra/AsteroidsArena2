module Asteroid where

import qualified Constants
import Graphics.Gloss (scale, translate)
import Physics (HasPhysics (..), PhysicsObject (..), accelerate, move)
import Player (Player)
import Rotation (rot, Angle)
import Sprites (baseAsteroid)
import System.Random (Random (..), RandomGen, StdGen)
import System.Random.Stateful (randomM)
import TypeClasses (Pictured (..), V2Math (..))
import VectorCalc (Point (Point))

type Size = Int

data Asteroid = Asteroid {phys :: PhysicsObject, size :: Size}

instance HasPhysics Asteroid where
  physobj = phys
  moveStep a dt = a {phys = move (phys a) dt}
  accelStep as dt a = as {phys = accelerate (phys as) dt a}

instance Pictured Asteroid where
  getGlobalPicture (Asteroid {phys = (PhysObj {position = t}), size = s}) = translate (x t) (y t) $ scale f f baseAsteroid
    where
      f = 2 ^ s

genRandomAsteroid :: StdGen -> Player -> (StdGen, Asteroid, Float)
genRandomAsteroid g0 p = (g, Asteroid (PhysObj pos vel rad) size, timeTillNext)
  where
    ((spawnAngle, moveAngle, size, uTime, moveSpeed), g) = randomR ((0, -60, 1, 0, 20), (360, 60, 3, 1, 80)) g0
    timeTillNext = - Constants.asteroidSpawnAverageInterval * log uTime
    pos = position (physobj p) |+| (Constants.spawnDistance |*| rot spawnAngle (Point 1 0))
    vel = (rot moveAngle . (moveSpeed |*|) . normalize) (position (physobj p) |-| pos)
    rad = Constants.asteroidRadius * (2 ^ size)

getChildAsteroids :: (Angle, Float) -> Asteroid -> [Asteroid]
getChildAsteroids _ (Asteroid {size = 1}) = []
getChildAsteroids (angle, speed) (Asteroid {size = s, phys = phy}) = ca
  where
    -- ((angle, speed), rn) = randomR ((0, 50), (120, 100)) r
    ca = map (\a ->Asteroid {size = s - 1, phys = phy {velocity = (speed |*| rot (angle + a) (Point 1 0)) |+| velocity phy, radius = radius phy / 2}} ) [-120, 0, 120]
