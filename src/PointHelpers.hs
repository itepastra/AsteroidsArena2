module PointHelpers where

import Point (Point (Point))

zeroPoint :: Num a => Point a
zeroPoint = Point 0 0

yUnit :: Num a => Point a
yUnit = Point 0 1

xUnit :: Num a => Point a
xUnit = Point 1 0