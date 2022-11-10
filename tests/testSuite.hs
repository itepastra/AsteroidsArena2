{-# LANGUAGE GADTs #-}

module Main where

import AFunctions (collapse, createFunc, fromString, getCarr, simplify, toString)
import Arbitrary ()
import Data.Foldable (maximumBy)
import Test.QuickCheck (Arbitrary, Discard (..), Property, Testable (property), counterexample, quickCheck, withMaxSuccess, within, (===))
import Types1 (AFunction, ElapsedTime)

prop_stringConversion :: AFunction Float -> Property
prop_stringConversion f = fromString (toString f) === Just f

prop_simplify :: AFunction Float -> ElapsedTime -> Property
prop_simplify f et = case createFunc f et of
  c
    | c /= c -> property Discard
    | otherwise -> approxEEE f co c $ createFunc co et
    where
      co = simplify f

prop_collapse :: AFunction Float -> ElapsedTime -> Property
prop_collapse f et = case createFunc f et of
  c
    | c /= c -> property Discard
    | otherwise -> approxEEE f co c $ createFunc co et
    where
      co = collapse f

main :: IO ()
main = do
  putStrLn "string conversion"
  quickCheck prop_stringConversion
  putStrLn "simplify"
  quickCheck prop_simplify
  putStrLn "collapse"
  quickCheck . (within 1000000 . withMaxSuccess 10000) $ prop_collapse
  putStrLn "Done"

approxEEE :: (Arbitrary a, Show a, Fractional a, Real a, Floating a) => AFunction a -> AFunction a -> a -> a -> Property
approxEEE f1 f2 x y = counterexample (show f1 ++ interpret res ++ show f2 ++ "\n\n" ++ show x ++ interpret res ++ show y) res
  where
    res = x == y || abs (x - y) <= max (abs x * 0.0005) (max (0.0005 * absMaximum (getCarr f1)) (0.0005 * absMaximum (getCarr f2)))
    interpret True = " == "
    interpret False = " /= "

absMaximum :: (Foldable f, Num a, Ord a) => f a -> a
absMaximum = abs . maximumBy (\x y -> compare (abs x) (abs y))