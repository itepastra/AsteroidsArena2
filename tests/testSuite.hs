{-# LANGUAGE GADTs #-}

module Main where

import Arbitrary ()
import Data.Aeson (Value (String), decode, encode)
import Data.Foldable (maximumBy)
import Data.Map (Map)
import JSONfuncs ()
import Test.QuickCheck (Arbitrary, Discard (..), Property, Testable (property), counterexample, quickCheck, withMaxSuccess, within, (===))
import Types1 (ElapsedTime, X)
import VFunctions (VFunction, fromString, mkNumFunc, simplify)

prop_stringConversion :: VFunction X Float -> Property
prop_stringConversion f = fromString (show f) === Just f

prop_jsonConversion :: VFunction X Float -> Property
prop_jsonConversion f = counterexample (show $ encode f) (decode (encode f) == Just f)

prop_simplify :: (Eq a, Arbitrary a, Show a, Floating a, Real a, Ord b, Show b) => VFunction b a -> Map b a -> Property
prop_simplify f et = case mkNumFunc f et of
  c
    | c /= c -> property Discard
    | otherwise -> approxEEE f co c $ mkNumFunc co et
    where
      co = simplify f

prop_funcEQ :: (Eq a, Show a, Eq b, Show b) => VFunction b a -> Property
prop_funcEQ f = (===) f f

main :: IO ()
main = do
  putStrLn "string conversion"
  quickCheck prop_stringConversion
  quickCheck prop_jsonConversion
  putStrLn "simplify"
  quickCheck (prop_simplify :: VFunction X Float -> Map X Float -> Property)
  putStrLn "check if it stays equal"
  quickCheck (prop_funcEQ :: VFunction X String -> Property)
  putStrLn "Done"

approxEEE :: (Arbitrary a, Show a, Fractional a, Real a, Floating a, Show b) => VFunction b a -> VFunction b a -> a -> a -> Property
approxEEE f1 f2 x y = counterexample (show f1 ++ interpret res ++ show f2 ++ "\n\n" ++ show x ++ interpret res ++ show y) res
  where
    res = x == y || abs (x - y) <= 5
    interpret True = " == "
    interpret False = " /= "

absMaximum :: (Num a, Ord a) => [a] -> a
absMaximum = abs . maximumBy (\x y -> compare (abs x) (abs y)) . (1 :)