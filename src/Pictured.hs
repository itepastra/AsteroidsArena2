module Pictured where

import Asteroid (Asteroid (Asteroid, SpaceMine), size)
import Bullet (Bullet (Bullet, lifeTime))
import Colors (textColor)
import qualified Colors
import Data.List (intercalate)
import EditorModel (EditorState (elapsedTime, iwalls, timeMultiplier))
import Graphics.Gloss (Picture (Text), blank, circleSolid, color, green, rotate, scale, translate)
import Graphics.Gloss.Data.Picture (Picture (Pictures))
import Level (Level (name))
import Player (Player (Player))
import Rotation (Rotate, getAngle)
import Select (getAllSelected)
import Sprites
  ( baseAsteroid,
    baseBullet,
    baseExhaust,
    basePlayer,
    baseSpaceMine,
    baseWall,
    selectedWall,
  )
import TypeClasses (HasPhysics (getPhysObj), Pictured (..), V2Math (..))
import Types1 (ElapsedTime, OverlayText (..), PhysicsObject (position, velocity), Point (Point), Selected (..))
import Wall (InitWall, Wall (sFunc), createWall, point, selfMove)

instance Pictured a => Pictured (Maybe a) where
  getPicture Nothing = blank
  getPicture (Just x) = getPicture x

instance Pictured Asteroid where
  getPicture o@(SpaceMine {}) = mvRotPic o $ scale f f baseSpaceMine
    where
      f = 2 ^ size o
  getPicture o@(Asteroid {}) = mvRotPic o $ scale f f baseAsteroid
    where
      f = 2 ^ size o

instance Pictured Player where
  getPicture o@(Player {}) = mvRotPic o $ Pictures [baseExhaust (velocity $ getPhysObj o), basePlayer]

instance Pictured Wall where
  getPicture w = translate (x p) (y p) $ rotate (-a + 180) baseWall
    where
      p = point w
      a = getAngle w

instance Pictured EditorState where
  getPicture gs =
    Pictures
      [ viewWallsSelect (elapsedTime gs) (iwalls gs), -- display the walls
        color green $ circleSolid 20, -- display where the player is
        translate (-800) 400 $ color textColor $ scale 0.2 0.2 $ Text (intercalate "\n" $ map show $ getAllSelected $ iwalls gs), -- display the wall stats
        translate (-800) (-430) $ color textColor $ scale 0.2 0.2 $ Text (show (elapsedTime gs) ++ " @ " ++ show (timeMultiplier gs) )
      ]

viewWallsSelect :: ElapsedTime -> [Selected InitWall] -> Picture
viewWallsSelect et = Pictures . map (viewSelectedWall et)

viewSelectedWall :: ElapsedTime -> Selected InitWall -> Picture
viewSelectedWall et iw = case iw of
  NotSelected iw' -> translate (x (p iw')) (y (p iw')) $ rotate (-(a iw') + 180) baseWall
  Selected _ iw' -> translate (x (p iw')) (y (p iw')) $ rotate (-(a iw') + 180) selectedWall
  where
    p i = point (w i)
    a i = getAngle (w i)
    w iw'' = selfMove et $ createWall iw''

instance Pictured Bullet where
  getPicture o@(Bullet {}) = mvWithPhys o (baseBullet (lifeTime o))

instance Pictured Level where
  getPicture l = scale 0.3 0.3 $ Text (name l)

instance (Pictured a) => Pictured (Selected a) where
  getPicture l = (color lc . getPicture . val) l
    where
      lc = case l of
        NotSelected _ -> Colors.textColor
        Selected x _ -> Colors.rainbowGradientColor x

instance Pictured OverlayText where
  getPicture (OT s) = color Colors.textColor . translate (fromIntegral (-40 * length s)) (-50) . Text $ s
  getPicture (ST s) = color Colors.textColor . translate (fromIntegral (-20 * length s)) (-150) . scale 0.5 0.5 . Text $ s

mvWithPhys :: HasPhysics a => a -> Picture -> Picture
mvWithPhys o = translate x y
  where
    (Point x y) = position $ getPhysObj o

rotWithRot :: Rotate a => a -> Picture -> Picture
rotWithRot o = rotate (getAngle o)

mvRotPic :: (HasPhysics a, Rotate a) => a -> Picture -> Picture
mvRotPic o = mvWithPhys o . rotWithRot o