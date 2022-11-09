{-# LANGUAGE InstanceSigs #-}

module Main where

import AFunctions (AFunction (..), collapse, createFunc, fromString, simplify, toString)
import Control.Monad (liftM2)
import Data.Maybe (fromJust)
import GHC.Base (liftM)
import GHC.Float (isFloatNaN)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Discard (Discard), Property, Result, Testable (property), coarbitrary, counterexample, elements, generate, oneof, quickCheck, quickCheckAll, quickCheckResult, quickCheckWith, suchThat, variant, (===))
import Test.QuickCheck.Gen (Gen, sized)
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
                liftM2 ExpF sf arbitrary,
                fmap SinF sf
              ]
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
    | otherwise -> approxEEE c $ createFunc (simplify f) et

prop_collapse :: AFunction -> ElapsedTime -> Property
prop_collapse f et = case createFunc f et of
  c
    | c /= c -> property Discard
    | otherwise -> approxEEE c $ createFunc (collapse f) et

main = do
  putStrLn "string conversion"
  quickCheck prop_stringConversion
  putStrLn "simplify"
  quickCheck prop_simplify
  putStrLn "collapse"
  quickCheck prop_collapse
  putStrLn "Done"

approxEEE :: Float -> Float -> Property
approxEEE x y = counterexample (show x ++ interpret res ++ show y) res
  where
    res = x == y || abs (x - y) <= 0.005
    interpret True = " == "
    interpret False = " /= "
