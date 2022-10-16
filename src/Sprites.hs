module Sprites where

import qualified Constants
import Graphics.Gloss.Data.Picture (Picture (..), circleSolid, scale, translate, lineLoop, rectangleUpperSolid)
import System.Random (StdGen)
import VectorCalc (Point (Point))
import GHC.Float (float2Int)
import Data.Fixed (mod')
import Colors (playerColor, bulletColor, asteroidColor, starColor, wallColor)

basePlayer :: Picture
basePlayer = Color playerColor $ Polygon [(0, 20), (20, -15), (0, 0), (-20, -15)]

baseBullet :: Picture
baseBullet = Color bulletColor $ circleSolid Constants.bulletRadius

baseAsteroid :: Picture
baseAsteroid = Color asteroidColor $ scale 3 3 $ Polygon [(-2.08, 0.57), (-3.28, 1.95), (-1.72, 3.08), (0.3, 2.43), (1.66, 3.15), (1.06, 0.41), (4.04, 0.11), (2.52, -3.27), (0.26, -2.05), (-2.94, -2.71), (-2.64, -1.29), (-3.27, 0.27)]

baseStar :: Picture
baseStar = Color starColor $ scale 30 30 $ lineLoop [(0.5, 0.16246), (0.118034, 0.16246), (0, 0.525731), (-0.118034, 0.16246), (-0.5, 0.16246), (-0.190983, -0.0620541), (-0.309017, -0.425325), (0, -0.200811), (0.309017, -0.425325), (0.190983, -0.0620541)]

baseWall :: Picture
baseWall = Color wallColor $ rectangleUpperSolid 10000 10000

starrySky :: [Point] -> Picture
starrySky = translate (-xw/2) (-yh/2) . Pictures . map (\(Point a b) -> translate  (a `mod'` xw) (b `mod'` yh) baseStar)
  where
    xw = fromIntegral $ fst Constants.pageSize
    yh = fromIntegral $ snd Constants.pageSize