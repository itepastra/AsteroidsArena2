{-# LANGUAGE GADTs #-}

module Main where

import Arbitrary ()
import Data.Aeson (Value (String), decode, encode)
import Data.Foldable (maximumBy)
import Data.Map (Map)
import JSONfuncs ()
import Test.QuickCheck (Arbitrary, Discard (..), Property, Testable (property), counterexample, quickCheck, withMaxSuccess, within, (===))
import TypeClasses (V2Math (..))
import Types1 (ElapsedTime, Point (Point), Var)
import VFunctions (VFunction, fromString, mkNumFunc, simplify)

-- Vector math tests

prop_vAdd :: Point -> Point -> Property
prop_vAdd v1@(Point x1 y1) v2@(Point x2 y2) = v1 |+| v2 === Point (x1 + x2) (y1 + y2)

prop_dot :: Point -> Point -> Property
prop_dot v1@(Point x1 y1) v2@(Point x2 y2) = v1 |.| v2 === (x1 * x2) + (y1 * y2)

-- VFunction Tests

prop_stringConversion :: VFunction Float Var -> Property
prop_stringConversion f = fromString (show f) === Just f

prop_jsonConversion :: VFunction Float Var -> Property
prop_jsonConversion f = counterexample (show $ encode f) (decode (encode f) == Just f)

prop_simplify :: (Eq a, Eq b, Arbitrary b, RealFloat b, Show b, Show a, Ord a) => VFunction b a -> Map a b -> Property
prop_simplify f et = case mkNumFunc f et of
  c
    | isInfinite c -> property Discard
    | c /= c -> property Discard
    | otherwise -> approxEEE f co c $ mkNumFunc co et
    where
      co = simplify f

prop_funcEQ :: (Eq a, Show a, Eq b, Show b) => VFunction b a -> Property
prop_funcEQ f = (===) f f

main :: IO ()
main = do
  quickCheck prop_vAdd
  quickCheck prop_dot
  putStrLn "string conversion"
  quickCheck prop_stringConversion
  quickCheck prop_jsonConversion
  putStrLn "simplify"
  quickCheck (prop_simplify :: VFunction Float Var -> Map Var Float -> Property)
  putStrLn "check if it stays equal"
  quickCheck (prop_funcEQ :: VFunction String Var -> Property)
  putStrLn "Done"

approxEEE :: (Arbitrary a, Show a, Fractional a, Real a, Floating a, Show b) => VFunction a b -> VFunction a b -> a -> a -> Property
approxEEE f1 f2 x y = counterexample (show f1 ++ interpret res ++ show f2 ++ "\n\n" ++ show x ++ interpret res ++ show y) res
  where
    res = x == y || abs (x - y) <= 5
    interpret True = " == "
    interpret False = " /= "

absMaximum :: (Num a, Ord a) => [a] -> a
absMaximum = abs . maximumBy (\x y -> compare (abs x) (abs y)) . (1 :)