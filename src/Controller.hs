--   This module defines how the state changes
--   in response to time and user input
module Controller where

import Asteroid (Asteroid (Asteroid, phys, rotateSpeed, size), genRandomAsteroid, getChildAsteroids, track)
import Bullet (Bullet (..), updateLifetime)
import qualified Constants
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Data.Set (Set, delete, insert, member)
import GHC.Base (ap)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (KeySpace))
import Model (GameState (..), newPlayer)
import Physics (HasPhysics (getPhysObj), PhysicsObject (PhysObj, position), TimeStep, accelStep, checkCollision, moveStep, Acceleration)
import Player (Player (..), friction, lookAccel, shoot)
import Rotation (Angle, rotate)
import System.Random (Random (randomRs), StdGen, split)
import TypeClasses (V2Math (..))
import VectorCalc (Point (Point))
import Wall (Wall, totalAcceleration)

step :: Float -> GameState -> IO GameState
step = (pure .) . pureStep

-- | Handle one iteration of the game
pureStep :: Float -> GameState -> GameState
pureStep secs gstate@(GameState {}) =
  case ndp of
    Nothing -> DeathState {score = score gstate, previousState = gstate}
    Just pl ->
      gstate
        { player = pl,
          asteroids = rrna,
          bullets = tnb,
          timeSinceLastShot = tsl,
          walls = nw,
          timeTillNextAsteroid = ttna,
          rand = nnrand,
          score = snew,
          elapsedTime = elapsedTime gstate + secs
        }
  where
    nb = spawnedBullet ++ map (updateBullet secs (walls gstate)) (filter (\(Bullet {lifeTime = l}) -> l > 0) (bullets gstate))
    na = map (updateAsteroid secs (player gstate)) (filter (\a -> (position . getPhysObj) a |#| (position . getPhysObj . player) gstate <= Constants.asteroidDespawnRange2) (asteroids gstate))
    np = updatePlayer rotspeed (walls gstate) secs (if member (Char 'w') (keys gstate) then lookAccel (player gstate) else Point 0 0) $ player gstate
    ndp = playerDamage na nb np
    nw = map (rotate (2 * secs)) (walls gstate)
    rotspeed
      | member (Char 'a') (keys gstate) = Constants.playerRotateSpeed
      | member (Char 'd') (keys gstate) = -Constants.playerRotateSpeed
      | otherwise = 0
    spawnedBullet = [shoot np | member (SpecialKey KeySpace) (keys gstate) && timeSinceLastShot gstate >= Constants.shootingInterval]
    tsl = case spawnedBullet of
      [] -> timeSinceLastShot gstate + secs
      _ -> 0
    (tnb, tna, da) = asteroidBulletCollisions nb na np
    (newrand, rna, ttna) = if timeTillNextAsteroid gstate <= 0 then (\(a, b, c) -> (a, b : tna, c)) $ genRandomAsteroid (rand gstate) (player gstate) else (rand gstate, tna, timeTillNextAsteroid gstate - secs)
    (arand, nnrand) = split newrand
    rrna = concatMap (uncurry getChildAsteroids) (zip (randomRs ((0, Constants.babyAsteroidMinimumSpeed, Constants.babyAsteroidMinimumRotation), (120, Constants.babyAsteroidMaximumSpeed, Constants.babyAsteroidMaximumRotation)) arand) da) ++ rna
    snew = score gstate + length da
pureStep secs gstate@(DeathState {}) = gstate
pureStep secs gstate@(MenuState {}) = gstate
pureStep secs gstate@(PauseState {}) = gstate

asteroidBulletCollisions :: [Bullet] -> [Asteroid] -> Player -> ([Bullet], [Asteroid], [Asteroid])
asteroidBulletCollisions [] [] _ = ([], [], [])
asteroidBulletCollisions bs as p =
  (filter (\b -> not (any (checkCollision b) as || checkCollision p b)) bs, la, da)
  where
    (la, da) = foldr (\a (as, ds) -> if checkCollision p a then (as, ds) else (if any (checkCollision a) bs then (as, a : ds) else (a : as, ds))) ([], []) as

playerDamage :: [Asteroid] -> [Bullet] -> Player -> Maybe Player
playerDamage as bs p = case foldl' (bulletDamage p) (foldl' (asteroidDamage p) (Just (hp p)) as) bs of
  Just health -> Just p {hp = health}
  Nothing -> Nothing
  where
    bulletDamage _ Nothing _ = Nothing
    bulletDamage p (Just hp) b = if checkCollision p b then (if hp > 15 then Just (hp - 15) else Nothing) else Just hp
    asteroidDamage _ Nothing _ = Nothing
    asteroidDamage p (Just hp) a = if checkCollision p a then (if hp > ad a then Just (hp - ad a) else Nothing) else Just hp
    ad a = 5 * 2 ^ size a

updateBullet :: TimeStep -> [Wall] -> Bullet -> Bullet
updateBullet secs walls = (\b -> updateLifetime secs . accelStep secs (totalAcceleration walls b) $ b) . moveStep secs

updateAsteroid :: TimeStep -> Player -> Asteroid -> Asteroid
updateAsteroid secs p = (\a -> rotate (secs * rotateSpeed a) a) . track p secs . moveStep secs

updatePlayer :: Angle -> [Wall] -> TimeStep -> Acceleration -> Player -> Player
updatePlayer rotspeed ws secs accel =
  rotate (rotspeed * secs)
    . friction secs
    . (\p -> accelStep secs  (accel |+| totalAcceleration ws p) p)
    . moveStep secs

-- ######### GEDAAN ##########
-- despawn bullets
-- bullets bewegen
-- despawn asteroids@
-- asteroids bewegen
-- player beweegt
-- player inputs
-- check if player and bullets are in force field
-- accelerate the player and bullets if necessary
-- delay between shots
-- check collisions between asteroids and bullets
-- spawn asteroids (spawn angle, speed angle, speed, timetillnext, picture angle, size)
-- check collisions between player and asteroids
-- check collisions player and bullets
-- fix baby asteroid spawning'

-- ######### TE DOEN ##########
-- levels
-- high scores
-- invurnerability frames maybe?
-- balancing
-- SPACE MINES
