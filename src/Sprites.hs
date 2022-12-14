module Sprites where

import Colors (asteroidColor, asteroidLineColor, bulletColor, exhaustGradientColor, playerColor, rainbowGradientColor, selectedWallColor, spaceMineColor, starColor, wallColor)
import qualified Constants
import Data.Fixed (mod')
import GHC.Float (float2Int)
import GeneralHelperFunctions (biFloat, scaleboth)
import Graphics.Gloss (color, green, makeColor)
import Graphics.Gloss.Data.Picture (Picture (..), arcSolid, circleSolid, lineLoop, polygon, rectangleUpperSolid, rotate, scale, translate)
import Point (Point (Point))
import System.Random (StdGen)
import Types1 (Velocity)
import VectorCalc ((|.|))

basePlayer :: Picture
basePlayer = Color playerColor $ Polygon [(0, 20), (20, -15), (0, 0), (-20, -15)]

baseBullet :: Float -> Picture
baseBullet a = Color (rainbowGradientColor (90 * a)) $ circleSolid (Constants.bulletRadius * (1 - (((Constants.bulletLifetime - a) / Constants.bulletLifetime) ^ 32)))

baseAsteroid :: Picture
baseAsteroid = scaleboth 3 $ Pictures [outline, fill]
  where
    outline = Color asteroidLineColor $ lineLoop poly
    fill = Color asteroidColor $ polygon poly
    poly = [(-2.08, 0.57), (-3.28, 1.95), (-1.72, 3.08), (0.3, 2.43), (1.66, 3.15), (1.06, 0.41), (4.04, 0.11), (2.52, -3.27), (0.26, -2.05), (-2.94, -2.71), (-2.64, -1.29), (-3.27, 0.27)]

baseSpaceMine :: Picture
baseSpaceMine = Color spaceMineColor $ polygon poly
  where
    poly = map (\(x, y) -> (x * Constants.asteroidRadius, y * Constants.asteroidRadius)) [(-0.6, 0), (-1, 1), (0, 0.6), (1, 1), (0.6, 0), (1, -1), (0, -0.6), (-1, -1)]

baseStar :: Picture
baseStar = Color starColor $ scaleboth 60 $ polygon [(0.190983, -6.20541e-2), (0.309017, -0.425325), (0.0, -0.200811), (-0.309017, -0.425325), (-0.190983, -6.20541e-2), (-0.5, 0.16246), (-0.118034, 0.16246), (0.0, 0.525731), (0.118034, 0.16246), (0.5, 0.16246)]

baseWall :: Picture
baseWall = color wallColor $ rotate 180 $ rectangleUpperSolid 10000 10000

selectedWall :: Picture
selectedWall = Color selectedWallColor $ rotate 180 $ rectangleUpperSolid 10000 10000

starrySky :: Float -> [Point Float] -> Picture
starrySky f = translate (-xw / 2) (-yh / 2) . Pictures . map (\(Point a b) -> translate (a `mod'` xw) (b `mod'` yh) $ scaleboth f baseStar)
  where
    (xw, yh) = biFloat Constants.pageSize
    sf a = -a / 2

baseExhaust :: Velocity -> Picture
baseExhaust a = Color (exhaustGradientColor b) $ arcSolid 216.87 323.13 c
  where
    b = sqrt (a |.| a)
    c
      | b <= 25 = 5
      | b >= 400 = 20
      | otherwise = sqrt b
