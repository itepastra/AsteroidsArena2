module GeneralHelperFunctions where

import Data.Bifunctor (Bifunctor (bimap))
import Graphics.Gloss (Picture, scale, translate)
import Types1 (Point (Point))

tZip3 :: (t1 -> a, t2 -> b, t3 -> c) -> (t1, t2, t3) -> (a, b, c)
tZip3 (f1, f2, f3) (a, b, c) = (f1 a, f2 b, f3 c)

flipFlop :: Num a => [a] -> [a]
flipFlop = zipWith (*) (concat $ repeat [1, -1])

biFloat :: (Bifunctor p, Integral c, Num d) => p c c -> p d d
biFloat = biF fromIntegral

biF :: Bifunctor p => (c -> d) -> p c c -> p d d
biF f = bimap f f

scaleboth :: Float -> Picture -> Picture
scaleboth f = scale f f

translateP :: Point -> Picture -> Picture
translateP (Point x y) = translate x y