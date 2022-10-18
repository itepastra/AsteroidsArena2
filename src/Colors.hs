module Colors where
import Graphics.Gloss (Color, magenta, green, blue, yellow, makeColor, cyan)

playerColor :: Color
playerColor = magenta

bulletColor :: Color
bulletColor = green

asteroidColor :: Color
asteroidColor = cyan

starColor :: Color
starColor = yellow

wallColor :: Color
wallColor = makeColor 1 0 0 0.2

