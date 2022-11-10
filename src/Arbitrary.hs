module Arbitrary where

import Control.Monad (liftM2, liftM3)
import Test.QuickCheck (Arbitrary (..), CoArbitrary (..), Gen, chooseInt, elements, oneof, sized, suchThat)
import Types1 (AFunction (..))
import Wall (InitWall (..))

instance (Eq a, Arbitrary a) => Arbitrary (AFunction a) where
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
                fmap SinF (f' (min 1 (n `div` 2))),
                fmap AbsF sf,
                fmap SigF sf
              ]
        | otherwise = undefined
        where
          sf = f' (n `div` 2)

instance Arbitrary InitWall where
  arbitrary = liftM3 InitWall arbitrary arbitrary arbitrary

instance CoArbitrary InitWall where
  coarbitrary (InitWall a b c) = coarbitrary a . coarbitrary b . coarbitrary c

instance CoArbitrary a => CoArbitrary (AFunction a) where
  coarbitrary (C x) = coarbitrary x
  coarbitrary Etime = id
  coarbitrary (AddF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (MulF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (ExpF f1 b) = coarbitrary f1 . coarbitrary b
  coarbitrary (SubF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (SinF f1) = coarbitrary f1
  coarbitrary (AbsF f1) = coarbitrary f1
  coarbitrary (SigF f1) = coarbitrary f1
