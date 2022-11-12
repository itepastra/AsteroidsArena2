module Pictured
  ( Pictured (..),
    Picture (..),
    mvRotPic,
    mvWithPhys,
    rotWithRot,
    scale,
    translate,
    colorText,
    translateI,
  )
where

import qualified Colors
import Graphics.Gloss.Data.Picture (Picture (..), blank, color, rotate, scale, translate)
import Rotation (Rotate, getAngle)
import TypeClasses (HasPhysics, getPhysObj)
import Types1 (OverlayText (..), Selected (..), position)
import VectorCalc (x, y)

class Pictured a where
  getPicture :: a -> Picture

instance Pictured a => Pictured (Maybe a) where
  getPicture Nothing = blank
  getPicture (Just x) = getPicture x

instance Pictured a => Pictured [a] where
  getPicture = Pictures . map getPicture

instance (Pictured a) => Pictured (Selected a) where
  getPicture l = (color lc . getPicture . val) l
    where
      lc = case l of
        NotSelected _ -> Colors.textColor
        Selected x _ -> Colors.rainbowGradientColor x

instance Pictured OverlayText where
  getPicture (OT s) = (colorText . translateI (-40 * length s) (-50) . Text) s
  getPicture (ST s) = (colorText . translateI (-20 * length s) (-150) . scale 0.5 0.5 . Text) s

mvWithPhys :: HasPhysics a => a -> Picture -> Picture
mvWithPhys o = translate (x p) (y p)
  where
    p = position $ getPhysObj o

rotWithRot :: Rotate a => a -> Picture -> Picture
rotWithRot o = rotate (getAngle o)

mvRotPic :: (HasPhysics a, Rotate a) => a -> Picture -> Picture
mvRotPic o = mvWithPhys o . rotWithRot o

colorText :: Picture -> Picture
colorText = color Colors.textColor

translateI :: (Integral a, Integral b) => a -> b -> Picture -> Picture
translateI x y = translate (fromIntegral x) (fromIntegral y)