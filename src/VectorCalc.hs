module VectorCalc where

import FISQ (fisqrt)
import GHC.Base (liftA2)
import Point (Point (Point))

(|.|) :: Num a => Point a -> Point a -> a
(|.|) = (sum .) . (*)

(|#|) :: Num a => Point a -> Point a -> a
a |#| b = c |.| c where c = a - b

(|*|) :: Num a => a -> Point a -> Point a
a |*| v = pure a * v

x :: Point a -> a
x (Point a _) = a

y :: Point a -> a
y (Point _ a) = a

toTuple :: Point a -> (a, a)
toTuple v = (x v, y v)

normalize :: Floating a => Point a -> Point a
normalize a = (1 / sqrt (a |.| a)) |*| a

fromTuple :: (a, a) -> Point a
fromTuple (a, b) = Point a b
