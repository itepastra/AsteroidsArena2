module Colors where

import Graphics.Gloss (Color, addColors, black, blue, cyan, green, magenta, makeColor, mixColors, orange, red, yellow)
import Rotation (Angle)

playerColor :: Color
playerColor = magenta

bulletColor :: Color
bulletColor = green

asteroidColor :: Color
asteroidColor = makeColor 0 0.8 1.0 0.3

spaceMineColor :: Color
spaceMineColor = red

asteroidLineColor :: Color
asteroidLineColor = makeColor 0 0.8 1.0 1.0

starColor :: Color
starColor = yellow

wallColor :: Color
wallColor = makeColor 1 0 0 0.2

overlayColor :: Color
overlayColor = makeColor 0 0 0 0.4

textColor :: Color
textColor = makeColor 0.95 0.95 0.95 1

rainbowGradientColor :: Float -> Color
rainbowGradientColor h = makeColor r g b 1
  where
    sin2 a = sin (pi * (a / 180)) ^ 2
    r = sin2 (h + 90)
    g = sin2 (h + 210)
    b = sin2 (h + 330)

exhaustGradientColor :: Float -> Color
exhaustGradientColor s = addColors c1 c2
  where
    rRatio = 300
    bRatio = s
    c1 = mixColors rRatio bRatio yellow black
    c2 = mixColors rRatio bRatio black blue
