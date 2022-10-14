-- | This module defines how to turn
--   the game state into a picture
module View where

import Bullet (Bullet (Bullet))
import Graphics.Gloss
  ( Path,
    Picture (Color, Pictures, Polygon, Scale),
    blank,
    circleSolid,
    green,
    magenta,
    rotate,
    translate,
  )
import Model (GameState (..), newPlayer)
import Physics (HasPhysics (physobj), PhysicsObject (PhysObj, position))
import Player (Player (Player))
import Sprites (baseStar, starrySky)
import TypeClasses (Pictured (..), V2Math (..))
import VectorCalc (Point (Point))
import qualified TypeClasses as VectorCalc

view :: GameState -> Picture
view = viewPure

viewPure :: GameState -> Picture
viewPure gs@(GameState {player = p}) = Pictures (viewBackground gs :  [moveWorldToCenter (physobj p) $ Pictures [viewPlayer gs, viewBullets gs, viewAsteroids gs]])
viewPure _ = blank

viewBackground :: GameState -> Picture
viewBackground (GameState {player = p, starPositions = sps}) = starrySky $ map ((|-| position (physobj p)) . fromTuple) sps
viewBackground _ = blank

viewPlayer :: GameState -> Picture
viewPlayer (GameState {player = p}) = getGlobalPicture p
viewPlayer _ = blank

viewAsteroids :: GameState -> Picture
viewAsteroids (GameState {player = p, asteroids = as}) = (Pictures . map getGlobalPicture) as
viewAsteroids _ = blank

viewBullets :: GameState -> Picture
viewBullets (GameState {player = p, bullets = as}) = (Pictures . map getGlobalPicture) as
viewBullets _ = blank

type CamOffset = Point

moveWorldToCenter :: PhysicsObject -> Picture -> Picture
moveWorldToCenter (PhysObj {position = t}) = translate (-x t) (-y t)
