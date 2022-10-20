module Sprites where

import Colors (asteroidColor, bulletColor, playerColor, starColor, wallColor, asteroidLineColor)
import qualified Constants
import Data.Bifunctor (Bifunctor (bimap))
import Data.Fixed (mod')
import GHC.Float (float2Int)
import Graphics.Gloss.Data.Picture (Picture (..), circleSolid, lineLoop, rectangleUpperSolid, scale, translate, polygon)
import System.Random (StdGen)
import VectorCalc (Point (Point))

basePlayer :: Picture
basePlayer = Color playerColor $ Polygon [(0, 20), (20, -15), (0, 0), (-20, -15)]

baseBullet :: Picture
baseBullet = Color bulletColor $ circleSolid Constants.bulletRadius

baseAsteroid :: Picture
baseAsteroid = scale 3 3 $ Pictures [outline, fill]
  where outline = Color asteroidLineColor $ lineLoop poly
        fill = Color asteroidColor $ polygon poly
        poly = [(-2.08, 0.57), (-3.28, 1.95), (-1.72, 3.08), (0.3, 2.43), (1.66, 3.15), (1.06, 0.41), (4.04, 0.11), (2.52, -3.27), (0.26, -2.05), (-2.94, -2.71), (-2.64, -1.29), (-3.27, 0.27)]


baseStar :: Picture
baseStar = Color starColor $ scale 60 60 $ polygon [(0.190983,-6.20541e-2),(0.309017,-0.425325),(0.0,-0.200811),(-0.309017,-0.425325),(-0.190983,-6.20541e-2),(-0.5,0.16246),(-0.118034,0.16246),(0.0,0.525731),(0.118034,0.16246),(0.5,0.16246)]

baseWall :: Picture
baseWall = Color wallColor $ rectangleUpperSolid 10000 10000

starrySky :: Float -> [Point] -> Picture
starrySky f = translate (-xw / 2) (-yh / 2) . Pictures . map (\(Point a b) -> translate (a `mod'` xw) (b `mod'` yh) $ scale f f baseStar)
  where
    (xw, yh) = bimap fromIntegral fromIntegral Constants.pageSize
