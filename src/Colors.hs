module Colors where
import Graphics.Gloss (Color, magenta, green, blue, yellow, makeColor)

playerColor :: Color
playerColor = magenta

bulletColor :: Color
bulletColor = green

asteroidColor :: Color
asteroidColor = blue

starColor :: Color
starColor = yellow

wallColor :: Color
wallColor = makeColor 1 0 0 0.2

