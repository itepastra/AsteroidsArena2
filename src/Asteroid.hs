module Asteroid where

import qualified Constants
import Data.Fixed (mod')
import Graphics.Gloss (rotate, scale, translate)
import Physics
  ( PhysicsObject (..),
    accelStep,
    frictionStep,
    updatePhysObj,
  )
import PointHelpers (xUnit)
import Rotation (Angle, Rotate (..), rot)
import Sprites (baseAsteroid, baseSpaceMine)
import System.Random (Random (..), RandomGen, StdGen)
import System.Random.Stateful (randomM)
import TypeClasses (HasPhysics (..), Pictured (..))
import Types1 (ElapsedTime, IntervalTime, Point (Point), Size, TimeStep, UniformTime)
import VectorCalc ( V2Math((|+|)), (|-|), (|#|), (|*|), normalize )

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
  setPhysObj po a = a {phys = po}

instance Rotate Asteroid where
  rotate a asteroid = asteroid {rotateAngle = (rotateAngle asteroid + a) `mod'` 360}
  getAngle = rotateAngle

genRandomAsteroid :: (UniformTime -> IntervalTime) -> Float -> StdGen -> PhysicsObject -> (StdGen, Asteroid, IntervalTime)
genRandomAsteroid t odds g0 p = (g, constr (PhysObj pos vel rad) size rSpeed rAngle, timeTillNext)
  where
    ((spawnAngle, moveAngle, size, uTime, moveSpeed, rSpeed, rAngle), g1) = randomR ((0, -25, 1, 0, 20, -15, 0), (360, 25, 3, 1, 80, 15, 360)) g0
    (atype, g) = randomR (0, 1) g1
    timeTillNext = t uTime
    pos = position p |+| (Constants.spawnDistance |*| rot spawnAngle xUnit)
    vel = (rot moveAngle . (moveSpeed |*|) . normalize) (position p |-| pos)
    rad = Constants.asteroidRadius * (2 ^ size)
    constr
      | atype < odds = SpaceMine
      | otherwise = Asteroid

getChildAsteroids :: RandomGen g => g -> Asteroid -> ([Asteroid] -> [Asteroid], g)
getChildAsteroids g a = ((getChildAsteroids' nums a ++), ng)
  where
    (nums, ng) =
      randomR
        ( ( 0,
            Constants.babyAsteroidMinimumSpeed,
            Constants.babyAsteroidMinimumRotation
          ),
          ( 120,
            Constants.babyAsteroidMaximumSpeed,
            Constants.babyAsteroidMaximumRotation
          )
        )
        g

getChildAsteroids' :: (Angle, Float, Float) -> Asteroid -> [Asteroid]
getChildAsteroids' _ (Asteroid {size = 1}) = []
getChildAsteroids' _ (SpaceMine {}) = []
getChildAsteroids' (angle, speed, rSpeed) (Asteroid {size = s, phys = phy, rotateAngle = ra, rotateSpeed = rss}) = ca
  where
    ca =
      zipWith
        ( \a rs ->
            Asteroid
              { size = s - 1,
                phys = phy {velocity = (speed |*| rot (angle + a) xUnit) |+| velocity phy, radius = radius phy / 2},
                rotateSpeed = rs + rss,
                rotateAngle = rs + ra
              }
        )
        [-120, 0, 120]
        [0, -rSpeed, rSpeed]

track :: PhysicsObject -> TimeStep -> Asteroid -> Asteroid
track p secs a@(Asteroid {}) = a
track p secs a@(SpaceMine {}) = frictionStep Constants.asteroidFrictionExponent secs . accelStep secs (((300 ^ 2) / (pp |#| pa)) |*| (pp |-| pa)) $ a
  where
    pp = position p
    pa = (position . getPhysObj) a

flipField :: PhysicsObject -> Asteroid -> Asteroid
flipField pp a
  | (position . getPhysObj) a |#| position pp <= Constants.asteroidDespawnRange2 = a
  | otherwise = updatePhysObj (\pa -> pa {position = position pp |-| (Constants.spawnDistance |*| normalize (position pa |-| position pp))}) a
