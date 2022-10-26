-- | This module defines how to turn
--   the game state into a picture
module View where

import Bullet (Bullet (Bullet))
import qualified Colors
import qualified Constants
import Data.Bifunctor (Bifunctor (bimap), second)
import Graphics.Gloss
  ( Path,
    Picture (Color, Pictures, Polygon, Scale, Text),
    addColors,
    black,
    blank,
    circleSolid,
    color,
    green,
    magenta,
    mixColors,
    rectangleSolid,
    rotate,
    scale,
    translate,
    white,
  )
import Model (GameState (..), newPlayer)
import Physics (HasPhysics (getPhysObj), PhysicsObject (PhysObj, position, velocity))
import Player (Player (Player, hp, phys))
import Sprites (baseStar, starrySky)
import TypeClasses (Pictured (..), V2Math (..))
import qualified TypeClasses as VectorCalc
import VectorCalc (Point (Point))

type CamOffset = Point

view :: GameState -> IO Picture
view = pure . viewPure

viewPure :: GameState -> Picture
viewPure gs@(GameState {player = p}) = Pictures [viewBackground gs, moveWorldToCenter (getPhysObj p) $ Pictures [viewWalls gs, viewPlayer gs, viewBullets gs, viewAsteroids gs], viewHud gs]
viewPure gs@(PauseState {}) = Pictures [viewPure (previousState gs), color Colors.overlayColor $ uncurry rectangleSolid $ bimap fromIntegral fromIntegral Constants.pageSize]
viewPure _ = blank

viewHud :: GameState -> Picture
viewHud (GameState {score = s, player = (Player {phys = (PhysObj {velocity = v}), hp = health})}) = Pictures (zipWith formatl [400, 350, 300] ["score: " ++ show s, "speed: " ++ show (sqrt (v |.| v)), "HP: " ++ show health])
  where
    formatl h = color Colors.textColor . translate (20 - fromIntegral (fst Constants.pageSize) / 2) h . Scale 0.3 0.3 . Text
viewHud _ = blank

viewBackground :: GameState -> Picture
viewBackground (GameState {player = p, starPositions = sps}) =
  Pictures
    ( zipWith
        ( \pax sp ->
            ( starrySky pax
                . map ((|-| (pax |*| position (getPhysObj p))) . fromTuple)
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

moveWorldToCenter :: PhysicsObject -> Picture -> Picture
moveWorldToCenter (PhysObj {position = t}) = translate (-x t) (-y t)
