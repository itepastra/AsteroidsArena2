{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import AFunctions (AFunction (..), collapse, createFunc, fromString, getCarr, simplify, toString)
import Control.Monad (liftM2)
import Data.Foldable (maximumBy)
import Data.Maybe (fromJust)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Discard (Discard), Fun, Property, Result, Testable (property), chooseInt, coarbitrary, counterexample, elements, generate, oneof, quickCheck, quickCheckAll, quickCheckResult, quickCheckWith, suchThat, variant, (===))
import Test.QuickCheck.Gen (Gen, sized)
import Test.QuickCheck.Property (withMaxSuccess)
import Types1 (AFunction, ElapsedTime)

instance Arbitrary AFunction where
  arbitrary :: Gen AFunction
  arbitrary =
    flip suchThat (\c -> c == c) $
      sized f'
    where
      f' 0 = oneof [fmap C arbitrary, elements [Etime]]
      f' n
        | n > 0 =
            oneof
              [ liftM2 AddF sf sf,
                liftM2 MulF sf sf,
                liftM2 SubF sf sf,
                liftM2 ExpF sf (chooseInt (-5, 5)),
                fmap SinF (f' (min 1 (n `div` 2)))
              ]
        | otherwise = undefined
        where
          sf = f' (n `div` 2)

instance CoArbitrary AFunction where
  coarbitrary (C x) = coarbitrary x
  coarbitrary Etime = id
  coarbitrary (AddF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (MulF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (ExpF f1 b) = coarbitrary f1 . coarbitrary b
  coarbitrary (SubF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (SinF f1) = coarbitrary f1

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