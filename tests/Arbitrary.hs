module Arbitrary where

import Control.Monad (liftM2, liftM3)
import Test.QuickCheck (Arbitrary (..), CoArbitrary (..), Gen, arbitraryBoundedEnum, chooseInt, elements, oneof, sized, suchThat)
import Types1 (Var (X))
import VFunctions (DOp, SOp, VFunction (Constant, OneIn, ThreeIn, TwoIn, Variable))
import Wall (InitWall (..))
import Point (Point (Point))

instance (Eq a, Arbitrary a, Arbitrary b, Eq b) => Arbitrary (VFunction b a) where
  arbitrary =
    flip suchThat (\c -> c == c) $
      sized f'
    where
      f' 0 = oneof [fmap Constant arbitrary, fmap Variable arbitrary]
      f' n
        | n > 0 =
            oneof
              [ liftM2 OneIn arbitrary ssf,
                liftM3 TwoIn arbitrary sf sf
              ]
        | otherwise = undefined
        where
          sf = f' (n `div` 2)
          ssf = f' (min 5 (n `div` 3))

instance Arbitrary SOp where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DOp where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InitWall where
  arbitrary = liftM3 InitWall arbitrary arbitrary arbitrary

instance Arbitrary Var where
  arbitrary = pure X

instance CoArbitrary InitWall where
  coarbitrary (InitWall a b c) = coarbitrary a . coarbitrary b . coarbitrary c

instance CoArbitrary Var where
  coarbitrary _ = id

instance (CoArbitrary a, CoArbitrary b) => CoArbitrary (VFunction b a) where
  coarbitrary (Constant x) = coarbitrary x
  coarbitrary (Variable x) = coarbitrary x
  coarbitrary (OneIn op f1) = coarbitrary f1
  coarbitrary (TwoIn op f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (ThreeIn op f1 f2 f3) = coarbitrary f1 . coarbitrary f2 . coarbitrary f3

instance Arbitrary a => Arbitrary (Point a) where
  arbitrary = liftM2 Point arbitrary arbitrary