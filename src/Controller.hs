{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--   This module defines how the state changes
--   in response to time and user input

module Controller where

import Asteroid
  ( Asteroid (rotateSpeed),
    flipField,
    genRandomAsteroid,
    getChildAsteroids,
    track,
  )
import Bullet (Bullet (lifeTime), updateLifetime)
import qualified Constants
import Data.Bifunctor (Bifunctor (second), bimap, first)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set (Set, empty, member)
import Graphics.Gloss.Interface.IO.Game (Key (Char, SpecialKey), SpecialKey (KeySpace))
import Hasa (HasA (getA, setA), updateA)
import Level (Level, LevelConfig (asteroidSpawnFunction))
import LevelImport (cleanFileLevels)
import Model (GameState (..), gameStateFromLevel)
import Physics (accelStep, checkCollision, frictionStep, moveStep, updatePhysObj)
import qualified Physics as Asteroid
import qualified Physics as Player
import Player (Player (Player, hp, lookAngle, lookDirection), lookAccel, playerDamage, playerHeal, shoot)
import Rotation (Rotate (rotate))
import Select (getSelected, sTime, selectFirst)
import Stars (genStarPositions)
import System.Random (Random (random, randomRs), RandomGen (split), StdGen, getStdGen, randomR)
import TypeClasses (V2Math (..), HasPhysics (..))
import Types1 (Acceleration, ElapsedTime, Hud (..), IntervalTime, Selected (NotSelected, Selected, val), Time, TimeStep, Angle, Point (Point), PhysicsObject)
import Wall (Wall, selfMove, totalAcceleration)

step :: Float -> GameState -> IO GameState
step secs gstate@(MenuState {levels = []}) = do
  lvls <- cleanFileLevels
  rd <- getStdGen
  let weh = selectFirst $ sort $ map NotSelected lvls
  step secs $ gstate {levels = weh, selectedState = stateSelect weh rd, rand = rd}
step secs gstate = pure $ pureStep secs gstate

pureStep :: Float -> GameState -> GameState
pureStep secs gstate@(GameState {starPositions = []}) = pureStep secs gstate {starPositions = genStarPositions (rand gstate) Constants.starAmount}
pureStep secs gstate@(DeathState {previousState = g@(DeathState {})}) = gstate {previousState = pureStep (secs / timeSinceDeath gstate) (previousState g)}
pureStep secs gstate@(DeathState {}) = gstate {previousState = pureStep (secs / timeSinceDeath gstate) (previousState gstate), timeSinceDeath = timeSinceDeath gstate + secs}
pureStep secs gstate@(MenuState {}) =
  gstate
    { levels = sTime (secs * 90) (levels gstate),
      selectedState = case selectedState gstate of
        Nothing -> Nothing
        Just gs -> Just $ pureStep secs gs
    }
pureStep secs gstate@(PauseState {}) = gstate
pureStep secs gstate@(GameState {player = (Player {hp = 0})}) =
  DeathState
    { previousState =
        updateA (playerHeal 1) $
          updateA emptyKeys gstate {hud = Invisible},
      timeSinceDeath = 1
    }
pureStep secs gstate@(GameState {}) =
  gstate
    { player = newDamagedPlayer,
      asteroids = rrna,
      bullets = trueNewBullets,
      timeSinceLastShot = newTimeSinceLast,
      walls = map (selfMove (elapsedTime gstate)) (walls gstate),
      timeTillNextAsteroid = ttna,
      rand = nnrand,
      score = snew,
      elapsedTime = elapsedTime gstate + secs,
      frameTime = secs
    }
  where
    -- move the player, bullets and asteroids
    (newTimeSinceLast, newBullets, newAsteroids, newPlayer) = positionUpdateStage secs gstate
    -- check player damage and the bullets that remain
    (newDamagedPlayer, trueNewBullets) = bulletPlayerUpdateStage newAsteroids newBullets newPlayer (hud gstate)
    -- remove, and spawn new asteroids
    (ttna, rrna, nnrand, sInc) = asteroidUpdateStage2 newBullets newPlayer gstate secs newAsteroids
    -- if the player is visible, update the score
    snew = case hud gstate of
      Visible -> score gstate + sInc
      Invisible -> score gstate

childAsteroids :: RandomGen g => g -> [Asteroid] -> ([Asteroid], g)
childAsteroids g = foldl (\(as, g) a -> first ($ as) (getChildAsteroids g a)) ([], g)

positionUpdateStage :: TimeStep -> GameState -> (Float, [Bullet], [Asteroid], Player)
positionUpdateStage secs gstate =
  uncurry
    (,,,)
    (second bulletsUpdate (bulletSpawn keyset (timeSinceLastShot gstate) secs playr))
    (map (updateAsteroid secs (getPhysObj playr)) (asteroids gstate))
    (updatePlayer (rotSpeed keyset) wallarr secs (acc keyset) playr)
  where
    bulletsUpdate spawnedBullet = mapMaybe (updateBullet secs wallarr) (spawnedBullet (bullets gstate))
    playr = player gstate
    keyset = keys gstate
    wallarr = walls gstate
    acc :: Set Key -> Acceleration
    acc k
      | member (Char 'w') k = lookAccel playr
      | otherwise = Point 0 0

bulletPlayerUpdateStage :: [Asteroid] -> [Bullet] -> Player -> Hud -> (Player, [Bullet])
bulletPlayerUpdateStage as bs p h = (dp, bulletCollisions as p bs)
  where
    dp = case h of
      Visible -> playerDamage as bs p
      Invisible -> p

asteroidUpdateStage2 :: [Bullet] -> Player -> GameState -> IntervalTime -> [Asteroid] -> (IntervalTime, [Asteroid], StdGen, Int)
asteroidUpdateStage2 bs p gstate secs as =
  (\((r, as, i), ds) -> (\(a, (b, d)) -> (a, b, d, length ds)) (i, first (++ as) (childAsteroids r ds))) $
    first
      ( spawnNewAsteroid
          (levelConfig gstate)
          (elapsedTime gstate)
          secs
          (timeTillNextAsteroid gstate)
          (rand gstate)
          (getPhysObj p)
      )
      (asteroidCollisions bs p as)

bulletCollisions :: (HasPhysics a, HasPhysics b) => [a] -> b -> [Bullet] -> [Bullet]
bulletCollisions as p = filter (\b -> not (any (checkCollision b) as || checkCollision p b))

asteroidCollisions :: (HasPhysics a, HasPhysics b) => [a] -> b -> [Asteroid] -> ([Asteroid], [Asteroid])
asteroidCollisions bs p = foldr (\a (as, ds) -> if checkCollision p a then (as, ds) else (if any (checkCollision a) bs then (as, a : ds) else (a : as, ds))) ([], [])

spawnNewAsteroid :: LevelConfig -> ElapsedTime -> IntervalTime -> Time -> StdGen -> PhysicsObject -> [Asteroid] -> (StdGen, [Asteroid], IntervalTime)
spawnNewAsteroid lc et it t rng phy oas
  | t <= 0 = (\(a, b, c) -> (a, b : oas, c)) $ genRandomAsteroid (asteroidSpawnFunction lc et) rng phy
  | otherwise = (rng, oas, t - it)

instance HasA Player GameState where
  getA :: GameState -> Player
  getA = player
  setA :: Player -> GameState -> GameState
  setA p g = g {player = p}

instance HasA (Set Key) GameState where
  getA :: GameState -> Set Key
  getA = keys
  setA :: Set Key -> GameState -> GameState
  setA k g = g {keys = k}

emptyKeys :: Set Key -> Set Key
emptyKeys = const empty

updateBullet :: TimeStep -> [Wall] -> Bullet -> Maybe Bullet
updateBullet secs walls b
  | lifeTime b <= 0 = Nothing
  | otherwise = Just (((accelStep secs =<< totalAcceleration walls) . updateLifetime secs . moveStep secs) b)

updateAsteroid :: TimeStep -> PhysicsObject -> Asteroid -> Asteroid
updateAsteroid secs p = (rotate =<< (secs *) . rotateSpeed) . track p secs . moveStep secs . flipField p

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

stateSelect :: [Selected Level] -> StdGen -> Maybe GameState
stateSelect x d = case getSelected x of
  Nothing -> Nothing
  Just le -> Just $ gameStateFromLevel d le

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
-- check collisions between player and asteroids
-- check collisions player and bullets
-- fix baby asteroid spawning'
-- SPACE MINES

-- ######### TE DOEN ##########
-- levels
-- high scores
-- invurnerability frames maybe?
-- balancing????
