module Sprites where

import Colors (asteroidColor, asteroidLineColor, bulletColor, exhaustGradientColor, playerColor, rainbowGradientColor, starColor, wallColor, spaceMineColor)
import qualified Constants
import Data.Bifunctor (Bifunctor (bimap))
import Data.Fixed (mod')
import GHC.Float (float2Int)
import Graphics.Gloss.Data.Picture (Picture (..), arcSolid, circleSolid, lineLoop, polygon, rectangleUpperSolid, scale, translate)
import Physics (Velocity)
import System.Random (StdGen)
import TypeClasses (V2Math ((|.|)))
import VectorCalc (Point (Point))
import Graphics.Gloss (green, color, makeColor)

basePlayer :: Picture
basePlayer = Color playerColor $ Polygon [(0, 20), (20, -15), (0, 0), (-20, -15)]

baseBullet :: Float -> Picture
baseBullet a = Color (rainbowGradientColor (90 * a)) $ circleSolid (Constants.bulletRadius * (1 - (((Constants.bulletLifetime - a) / Constants.bulletLifetime) ^ 32)))

-- c*(1-((c-x)/c) ^ 32)

baseAsteroid :: Picture
baseAsteroid = Pictures [scale 3 3 $ Pictures [outline, fill],color (makeColor 0 0.8 0 0.5) $ circleSolid Constants.asteroidRadius]
  where
    outline = Color asteroidLineColor $ lineLoop poly
    fill = Color asteroidColor $ polygon poly
    poly = [(-2.08, 0.57), (-3.28, 1.95), (-1.72, 3.08), (0.3, 2.43), (1.66, 3.15), (1.06, 0.41), (4.04, 0.11), (2.52, -3.27), (0.26, -2.05), (-2.94, -2.71), (-2.64, -1.29), (-3.27, 0.27)]

baseStar :: Picture
baseStar = Color starColor $ scale 60 60 $ polygon [(0.190983, -6.20541e-2), (0.309017, -0.425325), (0.0, -0.200811), (-0.309017, -0.425325), (-0.190983, -6.20541e-2), (-0.5, 0.16246), (-0.118034, 0.16246), (0.0, 0.525731), (0.118034, 0.16246), (0.5, 0.16246)]

baseWall :: Picture
baseWall = Color wallColor $ rectangleUpperSolid 10000 10000

starrySky :: Float -> [Point] -> Picture
starrySky f = translate (-xw / 2) (-yh / 2) . Pictures . map (\(Point a b) -> translate (a `mod'` xw) (b `mod'` yh) $ scale f f baseStar)
  where
    (xw, yh) = bimap fromIntegral fromIntegral Constants.pageSize

baseExhaust :: Velocity -> Picture
baseExhaust a = Color (exhaustGradientColor b) $ arcSolid 216.87 323.13 c
  where
    b = mysqrt (a |.| a) 300
    c
      | b <= 25 = 5
      | b >= 400 = 20
      | otherwise = mysqrt b 15

mysqrt :: Fractional a => a -> a -> a
mysqrt x y =
  let z = x + y ^ 2
   in z / (4 * y) + x * y / z

spaceMine :: Picture
spaceMine = Color spaceMineColor $ circleSolid Constants.asteroidRadius