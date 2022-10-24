-- | This module defines how to turn
--   the game state into a picture
module View where

import Bullet (Bullet (Bullet))
import qualified Constants
import Graphics.Gloss
  ( Path,
    Picture (Color, Pictures, Polygon, Scale, Text),
    blank,
    circleSolid,
    green,
    magenta,
    rotate,
    scale,
    translate,
    white,
  )
import Model (GameState (..), newPlayer)
import Physics (HasPhysics (physobj), PhysicsObject (PhysObj, position, velocity))
import Player (Player (Player, phys))
import Sprites (baseStar, starrySky)
import TypeClasses (Pictured (..), V2Math (..))
import qualified TypeClasses as VectorCalc
import VectorCalc (Point (Point))

view :: GameState -> IO Picture
view = pure . viewPure

viewPure :: GameState -> Picture
viewPure gs@(GameState {player = p}) = Pictures (viewBackground gs : Pictures [moveWorldToCenter (physobj p) $ Pictures [viewWalls gs, viewPlayer gs, viewBullets gs, viewAsteroids gs]] : [viewHud gs])
viewPure _ = blank

viewHud :: GameState -> Picture
viewHud (GameState {score = s, player = (Player {phys = (PhysObj {velocity = v})})}) = Pictures (zipWith formatl [400, 350] ["score: " ++ show s, "speed: " ++ show (sqrt (v |.| v))])
  where
    formatl h = Color white . translate (-430) h . Scale 0.3 0.3 . Text
viewHud _ = blank

viewBackground :: GameState -> Picture
viewBackground (GameState {player = p, starPositions = sps}) =
  Pictures
    ( zipWith
        ( \pax sp ->
            ( starrySky pax
                . map ((|-| (pax |*| position (physobj p))) . fromTuple)
            )
              sp
        )
        Constants.parallax
        sps
    )
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
