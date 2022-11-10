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
      f' 0 = oneof [fmap C arbitrary, elements [Var]]
      f' n
        | n > 0 =
            oneof
              [ liftM2 AddF sf sf,
                liftM2 MulF sf sf,
                liftM2 SubF sf sf,
                liftM2 DivF sf sf,
                fmap ExpF sf,
                fmap LogF sf,
                fmap AbsF sf,
                fmap SigF sf,
                fmap SinF ssf,
                fmap CosF ssf,
                fmap AsinF ssf,
                fmap AcosF ssf,
                fmap AtanF ssf,
                fmap AsinhF ssf,
                fmap AcoshF ssf,
                fmap AtanhF ssf
              ]
        | otherwise = undefined
        where
          sf = f' (n `div` 2)
          ssf = f' (min 5 (n `div` 3))

instance Arbitrary InitWall where
  arbitrary = liftM3 InitWall arbitrary arbitrary arbitrary

instance CoArbitrary InitWall where
  coarbitrary (InitWall a b c) = coarbitrary a . coarbitrary b . coarbitrary c

instance CoArbitrary a => CoArbitrary (AFunction a) where
  coarbitrary (C x) = coarbitrary x
  coarbitrary Var = id
  coarbitrary (MulF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (DivF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (SubF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (AddF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (ExpF f1) = coarbitrary f1
  coarbitrary (SinF f1) = coarbitrary f1
  coarbitrary (AbsF f1) = coarbitrary f1
  coarbitrary (SigF f1) = coarbitrary f1
  coarbitrary (LogF f1) = coarbitrary f1
  coarbitrary (CosF f1) = coarbitrary f1
  coarbitrary (AsinF f1) = coarbitrary f1
  coarbitrary (AcosF f1) = coarbitrary f1
  coarbitrary (AtanF f1) = coarbitrary f1
  coarbitrary (AsinhF f1) = coarbitrary f1
  coarbitrary (AcoshF f1) = coarbitrary f1
  coarbitrary (AtanhF f1) = coarbitrary f1
