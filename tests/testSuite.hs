{-# LANGUAGE GADTs #-}

module Main where

import Arbitrary ()
import Types1 (AFunction, ElapsedTime)
import Test.QuickCheck (Property, (===), Testable (property), Discard (..), quickCheck, withMaxSuccess, counterexample)
import AFunctions (fromString, toString, createFunc, simplify, collapse, getCarr)
import Data.Foldable (maximumBy)

prop_stringConversion :: AFunction -> Property
prop_stringConversion f = fromString (toString f) === Just f

prop_simplify :: AFunction -> ElapsedTime -> Property
prop_simplify f et = case createFunc f et of
  c
    | c /= c -> property Discard
    | otherwise -> approxEEE f co c $ createFunc co et
    where
      co = simplify f

prop_collapse :: AFunction -> ElapsedTime -> Property
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
  quickCheck (withMaxSuccess 10000 prop_collapse)
  putStrLn "Done"

approxEEE :: AFunction -> AFunction -> Float -> Float -> Property
approxEEE f1 f2 x y = counterexample (show f1 ++ interpret res ++ show f2 ++ "\n\n" ++ show x ++ interpret res ++ show y) res
  where
    res = x == y || abs (x - y) <= max (abs x * 0.0005) (max (0.0005 * absMaximum (getCarr f1)) (0.0005 * absMaximum (getCarr f2)))
    interpret True = " == "
    interpret False = " /= "

absMaximum :: (Foldable f, Num a, Ord a) => f a -> a
absMaximum = abs . maximumBy (\x y -> compare (abs x) (abs y))