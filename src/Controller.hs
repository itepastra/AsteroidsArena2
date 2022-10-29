{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--   This module defines how the state changes
--   in response to time and user input

module Controller where

import Asteroid
  ( Asteroid (rotateSpeed),
    genRandomAsteroid,
    getChildAsteroids,
    track,
  )
import AsteroidSpawnFunctions (divDecay, expDecay)
import Bullet (Bullet (lifeTime), updateLifetime)
import qualified Constants
import Data.Bifunctor (Bifunctor (second))
import Data.Maybe (mapMaybe)
import Data.Set (Set, empty, member)
import Graphics.Gloss.Interface.IO.Game (Key (Char, SpecialKey), SpecialKey (KeySpace))
import Hasa (HasA (getA, setA), updateA)
import Model (GameState (..))
import Physics (HasPhysics (getPhysObj), PhysicsObject (..), accelStep, checkCollision, frictionStep, moveStep, updatePhysObj)
import qualified Physics as Asteroid
import qualified Physics as Player
import Player (Player (InvPlayer, Player, hp, lookAngle, lookDirection), lookAccel, playerDamage, playerHeal, shoot, swapPlayerType)
import Rotation (Angle, Rotate (rotate))
import System.Random (Random (random, randomRs), RandomGen (split))
import TypeClasses (V2Math (..))
import Types1 (Acceleration, TimeStep)
import VectorCalc (Point (Point))
import Wall (Wall, totalAcceleration)

step :: Float -> GameState -> IO GameState
step = (pure .) . pureStep

-- | Handle one iteration of the game
pureStep :: Float -> GameState -> GameState
pureStep secs gstate@(DeathState {previousState = g@(DeathState {})}) = gstate {previousState = pureStep (secs / timeSinceDeath gstate) (previousState g)}
pureStep secs gstate@(DeathState {}) = gstate {previousState = pureStep (secs / timeSinceDeath gstate) (previousState gstate), timeSinceDeath = timeSinceDeath gstate + secs}
pureStep secs gstate@(MenuState {}) = gstate
pureStep secs gstate@(PauseState {}) = gstate
pureStep secs gstate@(GameState {player = InvPlayer {}}) = pureStep secs $ updateA (playerHeal 1) $ gstate {player = swapPlayerType (player gstate)}
pureStep secs gstate@(GameState {player = Player {}}) =
  case hp (player gstate) of
    0 ->
      DeathState
        { previousState =
            updateA emptyKeys $
              ( \g ->
                  g
                    { player =
                        swapPlayerType (player gstate)
                    }
              )
                gstate,
          timeSinceDeath = 1
        }
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
          elapsedTime = elapsedTime gstate + secs,
          frameTime = secs
        }
  where
    (newTimeSinceLast, newBullets) = second (($ bullets gstate) . (. mapMaybe (updateBullet secs (walls gstate)))) (bulletSpawn (keys gstate) (timeSinceLastShot gstate) secs (player gstate))
    newAsteroids = map (updateAsteroid secs ((getPhysObj . player) gstate)) (filter (\a -> (position . getPhysObj) a |#| (position . getPhysObj . player) gstate <= Constants.asteroidDespawnRange2) (asteroids gstate))
    newPlayer = updatePlayer (rotSpeed $ keys gstate) (walls gstate) secs (if member (Char 'w') (keys gstate) then lookAccel (player gstate) else Point 0 0) $ player gstate
    newDamagedPlayer = playerDamage newAsteroids newBullets newPlayer
    newWalls = map (rotate (2 * secs)) (walls gstate)
    trueNewBullets = bulletCollisions newAsteroids newPlayer newBullets
    (trueNewAsteroids, destroyedAsteroids) = asteroidCollisions newBullets newPlayer newAsteroids
    (newrand, rna, ttna) = if timeTillNextAsteroid gstate <= 0 then (\(a, b, c) -> (a, b : trueNewAsteroids, c)) $ genRandomAsteroid (expDecay Constants.asteroidSpawnAverageInterval (elapsedTime gstate)) (rand gstate) ((getPhysObj . player) gstate) else (rand gstate, trueNewAsteroids, timeTillNextAsteroid gstate - secs)
    nnrand
      | null destroyedAsteroids = newrand
      | otherwise = snd (split newrand)
    rrna = concatMap (uncurry getChildAsteroids) (zip (randomRs ((0, Constants.babyAsteroidMinimumSpeed, Constants.babyAsteroidMinimumRotation), (120, Constants.babyAsteroidMaximumSpeed, Constants.babyAsteroidMaximumRotation)) newrand) destroyedAsteroids) ++ rna
    snew = score gstate + length destroyedAsteroids

bulletCollisions :: (HasPhysics a, HasPhysics b) => [a] -> b -> [Bullet] -> [Bullet]
bulletCollisions as p = filter (\b -> not (any (checkCollision b) as || checkCollision p b))

asteroidCollisions :: (HasPhysics a, HasPhysics b) => [a] -> b -> [Asteroid] -> ([Asteroid], [Asteroid])
asteroidCollisions bs p = foldr (\a (as, ds) -> if checkCollision p a then (as, ds) else (if any (checkCollision a) bs then (as, a : ds) else (a : as, ds))) ([], [])

instance HasA Player GameState where
  getA = player
  setA p g = g {player = p}

instance HasA (Set Key) GameState where
  getA = keys
  setA k g = g {keys = k}

emptyKeys :: Set Key -> Set Key
emptyKeys = const empty

updateBullet :: TimeStep -> [Wall] -> Bullet -> Maybe Bullet
updateBullet secs walls b
  | 0 >= lifeTime b = Nothing
  | otherwise = Just (((accelStep secs =<< totalAcceleration walls) . updateLifetime secs . moveStep secs) b)

updateAsteroid :: TimeStep -> PhysicsObject -> Asteroid -> Asteroid
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
-- SPACE MINES

-- ######### TE DOEN ##########
-- levels
-- high scores
-- invurnerability frames maybe?
-- balancing????
