-- | This module defines how to turn
--   the game state into a picture
module View where

import Bullet (Bullet (Bullet))
import qualified Colors
import qualified Constants
import Data.Bifunctor (Bifunctor (bimap), second)
import Data.Function ((&))
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
import Model (GameState (..), Level (name), newPlayer)
import Physics (HasPhysics (getPhysObj), PhysicsObject (PhysObj, position, velocity))
import Player (Player (Player, hp, phys))
import Sprites (baseStar, starrySky)
import TypeClasses (Pictured (..), V2Math (..))
import qualified TypeClasses as VectorCalc
import Types1 (Selected (..), Time, Hud (Visible, Invisible))
import VectorCalc (Point (Point))

type CamOffset = Point

view :: GameState -> IO Picture
view = pure . getPicture

viewHud :: GameState -> Picture
viewHud (DeathState {}) = blank
viewHud (GameState {score = s, player = (Player {phys = (PhysObj {velocity = v}), hp = health}), asteroids = as}) = Pictures (zipWith formatl [400, 350, 300, 250] ["score: " ++ show s, "speed: " ++ show (sqrt (v |.| v)), "HP: " ++ show health, "LastFrame: " ++ show (length as)])
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
viewPlayer (GameState {player = p}) = getPicture p
viewPlayer _ = blank

viewAsteroids :: GameState -> Picture
viewAsteroids (GameState {asteroids = as}) = (Pictures . map getPicture) as
viewAsteroids _ = blank

viewBullets :: GameState -> Picture
viewBullets (GameState {bullets = as}) = (Pictures . map getPicture) as
viewBullets _ = blank

viewWalls :: GameState -> Picture
viewWalls (GameState {walls = ws}) = (Pictures . map getPicture) ws
viewWalls _ = blank

moveWorldToCenter :: PhysicsObject -> Picture -> Picture
moveWorldToCenter (PhysObj {position = t}) = translate (-x t) (-y t)

viewDimmed :: Picture
viewDimmed = color Colors.overlayColor $ uncurry rectangleSolid $ bimap fromIntegral fromIntegral Constants.pageSize

viewOverlayText :: String -> Picture
viewOverlayText s = color Colors.textColor . translate (fromIntegral (-40 * length s)) (-50) . Text $ s

instance Pictured GameState where
  getPicture gs@(GameState {player = p, hud = Invisible}) = Pictures [viewBackground gs, moveWorldToCenter (getPhysObj p) $ Pictures [viewWalls gs, viewBullets gs, viewAsteroids gs]]
  getPicture gs@(GameState {player = p, hud = Visible}) = Pictures [viewBackground gs, moveWorldToCenter (getPhysObj p) $ Pictures [viewWalls gs, viewPlayer gs, viewBullets gs, viewAsteroids gs], viewHud gs]
  getPicture gs@(PauseState {}) = Pictures [getPicture (previousState gs), viewDimmed, viewOverlayText "Pause"]
  getPicture gs@(DeathState {}) = Pictures [getPicture (previousState gs), viewDimmed, viewOverlayText "U Ded"]
  getPicture gs@(MenuState {}) = Pictures [getPicture (selectedState gs), viewLevelSelect $ levels gs]

instance Pictured a => Pictured (Maybe a) where
  getPicture Nothing = blank
  getPicture (Just x) = getPicture x

viewLevelSelect :: [Selected Level] -> Picture
viewLevelSelect lvls = Pictures $ zipWith formatLevel [1 ..] lvls

formatLevel :: Int -> Selected Level -> Picture
formatLevel = (. viewLevel) . translate 0 . fromIntegral . (-) (snd Constants.pageSize `div` 2) . (50 *)

viewLevel :: Selected Level -> Picture
viewLevel l = color lc $ scale 0.3 0.3 $ Text (name (val l))
  where
    lc = case l of
      NotSelected _ -> white
      Selected x _ -> Colors.rainbowGradientColor x

