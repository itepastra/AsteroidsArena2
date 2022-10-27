--   This module defines how the state changes
--   in response to time and user input
module Controller where

import Asteroid (Asteroid (..), genRandomAsteroid, getChildAsteroids, track)
import Bullet (Bullet (..), updateLifetime)
import qualified Constants
import Data.Bifunctor (Bifunctor (second))
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Set (Set, member)
import Graphics.Gloss.Interface.IO.Game (Key (Char, SpecialKey), SpecialKey (KeySpace))
import Model (GameState (..))
import Physics (Acceleration, HasPhysics (getPhysObj), PhysicsObject (..), TimeStep, accelStep, checkCollision, frictionStep, moveStep, updatePhysObj)
import qualified Physics as Asteroid
import qualified Physics as Player
import Player (Player (hp), lookAccel, shoot)
import Rotation (Angle, Rotate (rotate))
import System.Random (Random (random, randomRs), RandomGen (split))
import TypeClasses (V2Math (..))
import VectorCalc (Point (Point))
import Wall (Wall, totalAcceleration)

step :: Float -> GameState -> IO GameState
step = (pure .) . pureStep

-- | Handle one iteration of the game
pureStep :: Float -> GameState -> GameState
pureStep secs gstate@(GameState {}) =
  case hp (player gstate) of
    0 -> DeathState {previousState = gstate}
    _ ->
      gstate
        { player = newDamagedPlayer,
          asteroids = rrna,
          bullets = trueNewBullets,
          timeSinceLastShot = newTimeSinceLast,
          walls = newWalls,
          timeTillNextAsteroid = ttna,
          rand = nnrand,
          score = snew,
          elapsedTime = elapsedTime gstate + secs
        }
  where
    (newTimeSinceLast, newBullets) = second (($ bullets gstate) . (. mapMaybe (updateBullet secs (walls gstate)))) (bulletSpawn (keys gstate) (timeSinceLastShot gstate) secs (player gstate))
    newAsteroids = map (updateAsteroid secs (player gstate)) (filter (\a -> (position . getPhysObj) a |#| (position . getPhysObj . player) gstate <= Constants.asteroidDespawnRange2) (asteroids gstate))
    newPlayer = updatePlayer (rotSpeed $ keys gstate) (walls gstate) secs (if member (Char 'w') (keys gstate) then lookAccel (player gstate) else Point 0 0) $ player gstate
    newDamagedPlayer = playerDamage newAsteroids newBullets newPlayer
    newWalls = map (rotate (2 * secs)) (walls gstate)
    trueNewBullets = bulletCollisions newAsteroids newPlayer newBullets
    (trueNewAsteroids, destroyedAsteroids) = asteroidCollisions newBullets newPlayer newAsteroids
    (newrand, rna, ttna) = if timeTillNextAsteroid gstate <= 0 then (\(a, b, c) -> (a, b : trueNewAsteroids, c)) $ genRandomAsteroid (rand gstate) (player gstate) else (rand gstate, trueNewAsteroids, timeTillNextAsteroid gstate - secs)
    nnrand
      | not (null destroyedAsteroids) = snd (split newrand)
      | otherwise = newrand
    rrna = concatMap (uncurry getChildAsteroids) (zip (randomRs ((0, Constants.babyAsteroidMinimumSpeed, Constants.babyAsteroidMinimumRotation), (120, Constants.babyAsteroidMaximumSpeed, Constants.babyAsteroidMaximumRotation)) newrand) destroyedAsteroids) ++ rna
    snew = score gstate + length destroyedAsteroids
pureStep secs gstate@(DeathState {}) = gstate
pureStep secs gstate@(MenuState {}) = gstate
pureStep secs gstate@(PauseState {}) = gstate

bulletCollisions :: [Asteroid] -> Player -> [Bullet] -> [Bullet]
bulletCollisions as p = filter (\b -> not (any (checkCollision b) as || checkCollision p b))

asteroidCollisions :: [Bullet] -> Player -> [Asteroid] -> ([Asteroid], [Asteroid])
asteroidCollisions bs p = foldr (\a (as, ds) -> if checkCollision p a then (as, ds) else (if any (checkCollision a) bs then (as, a : ds) else (a : as, ds))) ([], [])

playerDamage :: [Asteroid] -> [Bullet] -> Player -> Player
playerDamage as bs p = np
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

updateBullet :: TimeStep -> [Wall] -> Bullet -> Maybe Bullet
updateBullet secs walls b
  | 0 >= lifeTime b = Nothing
  | otherwise = Just (((accelStep secs =<< totalAcceleration walls) . updateLifetime secs . moveStep secs) b)

updateAsteroid :: TimeStep -> Player -> Asteroid -> Asteroid
updateAsteroid secs p = (rotate =<< (secs *) . rotateSpeed) . track p secs . moveStep secs

updatePlayer :: Angle -> [Wall] -> TimeStep -> Acceleration -> Player -> Player
updatePlayer rotspeed ws secs accel =
  rotate (rotspeed * secs)
    . frictionStep Constants.playerFrictionExponent secs
    . (\p -> accelStep secs (accel |+| totalAcceleration ws p) p)
    . moveStep secs

rotSpeed :: Set Key -> Float
rotSpeed k
  | member (Char 'a') k = Constants.playerRotateSpeed
  | member (Char 'd') k = -Constants.playerRotateSpeed
  | otherwise = 0

bulletSpawn :: Set Key -> Float -> TimeStep -> Player -> (Float, [Bullet] -> [Bullet])
bulletSpawn ks ts secs p
  | member (SpecialKey KeySpace) ks && ts >= Constants.shootingInterval = (0, (shoot p :))
  | otherwise = (ts + secs, id)

-- updateAsteroid = ap ((.) . (.) . join . (rotate .) . (. rotateSpeed) . (*)) (ap (flip . ((.) .) . flip track) moveStep)
-- updateAsteroid secs = ((rotate =<< (secs *) . rotateSpeed) .) . (. moveStep secs) . flip track secs

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
