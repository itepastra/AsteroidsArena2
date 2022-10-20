module Colors where
import Graphics.Gloss (Color, magenta, green, blue, yellow, makeColor, cyan)

playerColor :: Color
playerColor = magenta

bulletColor :: Color
bulletColor = green

asteroidColor :: Color
asteroidColor = makeColor 0 0.8 1.0 0.3

asteroidLineColor :: Color
asteroidLineColor = makeColor 0 0.8 1.0 1.0

starColor :: Color
starColor = yellow

wallColor :: Color
wallColor = makeColor 1 0 0 0.2

