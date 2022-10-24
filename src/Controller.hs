--   This module defines how the state changes
--   in response to time and user input
module Controller where

import Asteroid (Asteroid (Asteroid, phys, rotateSpeed, size), genRandomAsteroid, getChildAsteroids)
import Bullet (Bullet (..), updateLifetime)
import qualified Constants
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Data.Set (Set, delete, insert, member)
import GHC.Base (ap)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (KeySpace))
import Model (GameState (..))
import Physics (HasPhysics (accelStep, moveStep, physobj), PhysicsObject (PhysObj, position), checkCollision)
import Player (Player (..), friction, lookAccel, shoot)
import Rotation (rotate)
import System.Random (Random (randomRs), StdGen, split)
import TypeClasses (V2Math (..))
import VectorCalc (Point (Point))
import Wall (totalAcceleration)

step :: Float -> GameState -> IO GameState
step = (pure .) . pureStep

-- | Handle one iteration of the game
pureStep :: Float -> GameState -> GameState
pureStep secs gstate =
  gstate
    { player = np,
      asteroids = rrna,
      bullets = tnb,
      timeSinceLastShot = tsl,
      walls = nw,
      timeTillNextAsteroid = ttna,
      rand = newrand,
      score = snew
    }
  where
    nb = spawnedBullet ++ map ((\b -> updateLifetime secs . (b `accelStep` secs) . flip totalAcceleration (walls gstate) $ b) . (`moveStep` secs)) (filter (\(Bullet _ l) -> l > 0) (bullets gstate))
    na = map ((\a -> rotate (secs * rotateSpeed a) a) . (`moveStep` secs)) (filter (\(Asteroid {Asteroid.phys = (PhysObj {position = p})}) -> p |#| (position . physobj . player) gstate <= Constants.asteroidDespawnRange2) (asteroids gstate))
    np = rotate (rotspeed * secs) . friction secs . (\b -> (b `accelStep` secs) ((if member (Char 'w') (keys gstate) then lookAccel b else Point 0 0) |+| totalAcceleration b (walls gstate))) . (`moveStep` secs) $ player gstate
    nw = map (rotate (2 * secs)) (walls gstate)
    rotspeed
      | member (Char 'a') (keys gstate) = Constants.playerRotateSpeed
      | member (Char 'd') (keys gstate) = -Constants.playerRotateSpeed
      | otherwise = 0
    spawnedBullet = [shoot np | member (SpecialKey KeySpace) (keys gstate) && timeSinceLastShot gstate >= Constants.shootingInterval]
    tsl = case spawnedBullet of
      [] -> timeSinceLastShot gstate + secs
      _ -> 0
    (tnb, tna, da) = asteroidBulletCollisions nb na
    (newrand, rna, ttna) = if timeTillNextAsteroid gstate <= 0 then (\(a, b, c) -> (a, b : tna, c)) $ genRandomAsteroid (rand gstate) (player gstate) else (rand gstate, tna, timeTillNextAsteroid gstate - secs)
    rrna = concatMap (uncurry getChildAsteroids) (zip (randomRs ((0, Constants.babyAsteroidMinimumSpeed, Constants.babyAsteroidMinimumRotation), (120, Constants.babyAsteroidMaximumSpeed, Constants.babyAsteroidMaximumRotation)) (rand gstate)) da) ++ rna
    snew = score gstate + length da

asteroidBulletCollisions :: (HasPhysics a, HasPhysics b) => [a] -> [b] -> ([a], [b], [b])
asteroidBulletCollisions [] [] = ([], [], [])
asteroidBulletCollisions bs [] = (bs, [], [])
asteroidBulletCollisions [] as = ([], as, [])
asteroidBulletCollisions bs as =
  (filter (\b -> not $ any (checkCollision b) as) bs, la, da)
  where
    (la, da) = foldr (\a (as, ds) -> if any (checkCollision a) bs then (as, a : ds) else (a : as, ds)) ([], []) as

playerDamage :: Player -> [Asteroid] -> [Bullet] -> Maybe Player
playerDamage p as bs = case foldl' (bulletDamage p) (foldl' (asteroidDamage p) (Just (hp p)) as) bs of
        Just health -> Just p {hp = health}
        Nothing -> Nothing
  where
    bulletDamage _ Nothing _ = Nothing
    bulletDamage p (Just hp) b = if checkCollision p b then Just (hp - 15) else Just hp
    asteroidDamage _ Nothing _ = Nothing
    asteroidDamage p (Just hp) a = if checkCollision p a then Just (hp - 5*2^size a) else Just hp

-- ######### GEDAAN ##########
-- despawn bullets
-- bullets bewegen
-- despawn asteroids
-- asteroids bewegen
-- player beweegt
-- player inputs
-- check if player and bullets are in force field
-- accelerate the player and bullets if necessary
-- delay between shots
-- check collisions between asteroids and bullets
-- spawn asteroids (spawn angle, speed angle, speed, timetillnext, picture angle, size)

-- ######### TE DOEN ##########
-- check collisions between player and asteroids
-- check collisions player and bullets
-- fix baby asteroid spawning'

input :: Event -> GameState -> IO GameState
input = (pure .) . pureInput

pureInput :: Event -> GameState -> GameState
pureInput (EventKey k Down _ _) g = g {keys = insert k (keys g)}
pureInput (EventKey k Up _ _) g = g {keys = delete k (keys g)}
pureInput _ g = g