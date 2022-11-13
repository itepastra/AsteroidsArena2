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
    translateP,
    textFormatP,
    textFormat
  )
where

import qualified Colors
import GeneralHelperFunctions (scaleboth, translateI, translateP)
import Graphics.Gloss.Data.Picture (Picture (..), blank, color, rotate, scale, translate)
import Point (Point (Point))
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
  getPicture (ST s) = (colorText . translateI (-20 * length s) (-150) . scaleboth 0.5 . Text) s

mvWithPhys :: HasPhysics a => a -> Picture -> Picture
mvWithPhys = translateP . position . getPhysObj

rotWithRot :: Rotate a => a -> Picture -> Picture
rotWithRot o = rotate (getAngle o)

mvRotPic :: (HasPhysics a, Rotate a) => a -> Picture -> Picture
mvRotPic o = mvWithPhys o . rotWithRot o

colorText :: Picture -> Picture
colorText = color Colors.textColor

textFormatP :: Point Float -> Float -> String -> Picture
textFormatP (Point x y) = textFormat x y

textFormat :: Float -> Float -> Float -> String -> Picture
textFormat x y s = colorText . translate x y . scaleboth s . Text