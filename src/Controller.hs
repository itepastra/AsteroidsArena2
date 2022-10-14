--   This module defines how the state changes
--   in response to time and user input
module Controller where

import Asteroid (Asteroid (Asteroid))
import Bullet (Bullet (..), updateLifetime)
import qualified Constants
import Data.Set (Set, delete, insert, member)
import GHC.Base (ap)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (KeySpace))
import Model (GameState (..))
import Physics (HasPhysics (accelStep, moveStep, physobj), PhysicsObject (PhysObj, position))
import Player (Player (..), lookAccel, shoot)
import Rotation (rotate)
import System.Random ()
import TypeClasses (V2Math (..))
import VectorCalc (Point (Point))
import Wall (totalAcceleration)

-- | Handle one iteration of the game
step :: Float -> GameState -> GameState
step secs gstate =
  gstate
    { player = np,
      asteroids = na,
      bullets = nb,
      timeSinceLastShot = tsl
    }
  where
    nb = spawnedBullet ++ map ((\b -> updateLifetime secs . (b `accelStep` secs) . flip totalAcceleration (walls gstate) $ b) . (`moveStep` secs)) (filter (\(Bullet _ l) -> l > 0) (bullets gstate))
    na = map (`moveStep` secs) (filter (\(Asteroid (PhysObj {position = p}) _) -> p |#| (position . physobj . player) gstate <= Constants.asteroidDespawnRange2) (asteroids gstate))
    np = rotate (rotspeed * secs) . (\b -> (b `accelStep` secs) ((if member (Char 'w') (keys gstate) then lookAccel secs b else Point 0 0) |+| totalAcceleration b (walls gstate))) . (`moveStep` secs) $ player gstate
    rotspeed
      | member (Char 'a') (keys gstate) = 180
      | member (Char 'd') (keys gstate) = -180
      | otherwise = 0
    spawnedBullet = [shoot np | member (SpecialKey KeySpace) (keys gstate) && timeSinceLastShot gstate >= Constants.shootingInterval]
    tsl = case spawnedBullet of
      [] -> timeSinceLastShot gstate + secs
      _ -> 0--

-- ######### GEDAAN ##########
-- despawn bullets
-- bullets bewegen
-- despawn asteroids
-- asteroids bewegen
-- player beweegt
-- player inputs
-- check if player and bullets are in force field
-- accelerate the player and bullets if necessary

-- ######### TE DOEN ##########
-- delay between shots
-- spawn asteroids
-- check collisions

input :: Event -> GameState -> GameState
input (EventKey k Down _ _) g = g {keys = insert k (keys g)}
input (EventKey k Up _ _) g = g {keys = delete k (keys g)}
input _ g = g

