-- | This module defines how to turn
--   the game state into a picture
module View where

import qualified Colors
import qualified Constants
import Data.Bifunctor (Bifunctor (bimap))
import Graphics.Gloss (Picture (..), blank, blue, color, rectangleSolid, rotate, scale, translate)
import Level (Level (..))
import Model (GameState (..), gameStateFromLevel)
import Physics (PhysicsObject (..))
import Pictured ()
import Player (Player (hp))
import Rotation (Rotate (getAngle))
import Select (getSingleSelected, getSelectedIndex)
import Sprites (selectedWall, starrySky)
import System.Random (mkStdGen)
import TypeClasses (HasPhysics (getPhysObj), Pictured (..), V2Math (..))
import Types1 (ElapsedTime, Hud (..), OverlayText (..), Selected (..))
import Wall (InitWall, Wall, createWall, point, selfMove)

view :: GameState -> IO Picture
view = pure . getPicture

viewHud :: GameState -> Picture
viewHud gs@(GameState {score = s, player = p, timeTillNextAsteroid = ttna}) = Pictures (zipWith formatl [400, 350, 300, 250] ["score: " ++ show s, "speed: " ++ show (sqrt (v |.| v)), "HP: " ++ show health])
  where
    formatl h = color Colors.textColor . translate (20 - fromIntegral (fst Constants.pageSize) / 2) h . Scale 0.3 0.3 . Text
    v = velocity $ getPhysObj p
    health = hp p
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
viewPlayer = getPicture . player

viewAsteroids :: GameState -> Picture
viewAsteroids = Pictures . map getPicture . asteroids

viewBullets :: GameState -> Picture
viewBullets = Pictures . map getPicture . bullets

viewWalls :: GameState -> Picture
viewWalls = Pictures . map getPicture . walls

moveWorldToCenter :: PhysicsObject -> Picture -> Picture
moveWorldToCenter (PhysObj {position = t}) = translate (-x t) (-y t)

dimmedScreen :: Picture
dimmedScreen = color Colors.overlayColor $ uncurry rectangleSolid $ bimap fromIntegral fromIntegral Constants.pageSize

viewOverlayText :: String -> Picture
viewOverlayText = getPicture . OT

viewLevelSelect :: [Selected Level] -> Picture
viewLevelSelect lvls = Pictures $ zipWith formatLevel [n, (n - 1) ..] lvls
  where
    currentSelected = getSelectedIndex lvls
    n = case currentSelected of
      Nothing -> 3
      Just i -> 3 + i

formatLevel :: Int -> Selected Level -> Picture
formatLevel n = translate (-100) (fromIntegral (50 * n)) . getPicture

instance Pictured GameState where
  getPicture gs@(GameState {player = p, hud = Invisible}) = Pictures [viewBackground gs, moveWorldToCenter (getPhysObj p) $ Pictures [viewWalls gs, viewBullets gs, viewAsteroids gs]]
  getPicture gs@(GameState {player = p, hud = Visible}) = Pictures [viewBackground gs, moveWorldToCenter (getPhysObj p) $ Pictures [viewWalls gs, viewPlayer gs, viewBullets gs, viewAsteroids gs], viewHud gs]
  getPicture gs@(PauseState {}) = Pictures [getPicture (previousState gs), dimmedScreen, viewOverlayText "Pause"]
  getPicture gs@(DeathState {}) = Pictures [getPicture (previousState gs), dimmedScreen, viewOverlayText "Wrecked", getPicture . ST $ "Score: " ++ show (score $ previousState gs)]
  getPicture gs@(MenuState {}) = Pictures [getPicture (selectedState gs), viewLevelSelect $ levels gs]
