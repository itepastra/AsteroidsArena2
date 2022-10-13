-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Player (Player (Player))
import Physics (PhysicsObject(position))
import VectorCalc (V2Math(..))

view :: GameState ->  Picture
view = viewPure

viewPure :: GameState -> Picture
viewPure gs = viewPlayer (player gs)
-- viewPure gstate = case ShowNothing gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])

playerPath :: Path
playerPath = [(0,0), (4,-7), (0, -4), (-4,-7)]

basePlayer :: Picture
basePlayer = Color white $ Scale 5 5 $ Polygon playerPath

viewPlayer :: Player -> Picture
viewPlayer (Player phy _ _ a) = translate (x t) (y t) $ rotate a basePlayer
    where t = position phy