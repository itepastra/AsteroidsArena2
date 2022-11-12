module GeneralHelperFunctions where

import Data.Bifunctor (Bifunctor (bimap))

tZip3 :: (t1 -> a, t2 -> b, t3 -> c) -> (t1, t2, t3) -> (a, b, c)
tZip3 (f1, f2, f3) (a, b, c) = (f1 a, f2 b, f3 c)

flipFlop :: Num a => [a] -> [a]
flipFlop = zipWith (*) (concat $ repeat [1, -1])

biFloat :: (Integral a, Num b, Integral c, Num d, Bifunctor f) => f a c -> f b d
biFloat = bimap fromIntegral fromIntegral