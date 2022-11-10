{-# LANGUAGE GADTs #-}

module Main where

import AFunctions (collapse, createFunc, fromString, getCarr, simplify, toString)
import Arbitrary ()
import Data.Aeson (Value (String), decode, encode)
import Data.Foldable (maximumBy)
import JSONfuncs ()
import Test.QuickCheck (Arbitrary, Discard (..), Property, Testable (property), counterexample, quickCheck, withMaxSuccess, within, (===))
import Types1 (AFunction, ElapsedTime)

prop_stringConversion :: AFunction Float -> Property
prop_stringConversion f = fromString (toString f) === Just f

prop_jsonConversion :: AFunction Float -> Property
prop_jsonConversion f = decode (encode f) === Just f

prop_simplify :: AFunction Float -> ElapsedTime -> Property
prop_simplify f et = case createFunc f et of
  c
    | c /= c -> property Discard
    | otherwise -> approxEEE f co c $ createFunc co et
    where
      co = simplify f

prop_funcEQ :: (Eq a, Show a) => AFunction a -> Property
prop_funcEQ f = (===) f f

main :: IO ()
main = do
  putStrLn "string conversion"
  quickCheck prop_stringConversion
  quickCheck prop_jsonConversion
  putStrLn "simplify"
  quickCheck prop_simplify
  putStrLn "check if it stays equal"
  quickCheck (prop_funcEQ :: AFunction String -> Property)
  putStrLn "Done"

approxEEE :: (Arbitrary a, Show a, Fractional a, Real a, Floating a) => AFunction a -> AFunction a -> a -> a -> Property
approxEEE f1 f2 x y = counterexample (show f1 ++ interpret res ++ show f2 ++ "\n\n" ++ show x ++ interpret res ++ show y) res
  where
    res = x == y || abs (x - y) <= max (0.0005 * absMaximum (getCarr f1)) (0.05 * absMaximum (getCarr f2))
    interpret True = " == "
    interpret False = " /= "

absMaximum :: (Num a, Ord a) => [a] -> a
absMaximum = abs . maximumBy (\x y -> compare (abs x) (abs y)) . (1 :)