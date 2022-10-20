-- | This module defines how to turn
--   the game state into a picture
module View where

import Bullet (Bullet (Bullet))
import Graphics.Gloss
  ( Path,
    Picture (Color, Pictures, Polygon, Scale, Text),
    blank,
    circleSolid,
    green,
    magenta,
    rotate,
    translate, white, scale,
  )
import Model (GameState (..), newPlayer)
import Physics (HasPhysics (physobj), PhysicsObject (PhysObj, position))
import Player (Player (Player))
import Sprites (baseStar, starrySky)
import TypeClasses (Pictured (..), V2Math (..))
import VectorCalc (Point (Point))
import qualified TypeClasses as VectorCalc
import qualified Constants

view :: GameState -> IO Picture
view = pure . viewPure

viewPure :: GameState -> Picture
viewPure gs@(GameState {player = p}) = Pictures (viewBackground gs :  Pictures [moveWorldToCenter (physobj p) $ Pictures [viewWalls gs, viewPlayer gs, viewBullets gs, viewAsteroids gs]] : [viewHud gs])
viewPure _ = blank

viewHud :: GameState -> Picture
viewHud (GameState {score = s}) = Color white $ translate (-430) 400 $ Scale 0.3 0.3 $ Text ("score: " ++ show s)
viewHud _ = blank

viewBackground :: GameState -> Picture
viewBackground (GameState {player = p, starPositions = sps}) = Pictures (zipWith (\ pax sp
  -> ( starrySky pax
        . map ((|-| (pax |*| position (physobj p))) . fromTuple))
       sp) Constants.parallax sps)
viewBackground _ = blank

viewPlayer :: GameState -> Picture
viewPlayer (GameState {player = p}) = getGlobalPicture p
viewPlayer _ = blank

viewAsteroids :: GameState -> Picture
viewAsteroids (GameState {asteroids = as}) = (Pictures . map getGlobalPicture) as
viewAsteroids _ = blank

viewBullets :: GameState -> Picture
viewBullets (GameState {bullets = as}) = (Pictures . map getGlobalPicture) as
viewBullets _ = blank

viewWalls :: GameState -> Picture
viewWalls (GameState {walls = ws}) = (Pictures . map getGlobalPicture) ws
viewWalls _ = blank

type CamOffset = Point

moveWorldToCenter :: PhysicsObject -> Picture -> Picture
moveWorldToCenter (PhysObj {position = t}) = translate (-x t) (-y t)
