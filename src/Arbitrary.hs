{-# LANGUAGE InstanceSigs #-}
module Arbitrary where
import Test.QuickCheck (Arbitrary (..), Gen, CoArbitrary (..), suchThat, sized, oneof, elements, chooseInt)
import Types1 (AFunction (..))
import Wall (InitWall (..))
import Control.Monad (liftM2, liftM3)


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

instance Arbitrary InitWall where
  arbitrary :: Gen InitWall
  arbitrary = liftM3 InitWall arbitrary arbitrary arbitrary

instance CoArbitrary InitWall where
  coarbitrary (InitWall a b c) = coarbitrary a . coarbitrary b . coarbitrary c

instance CoArbitrary AFunction where
  coarbitrary (C x) = coarbitrary x
  coarbitrary Etime = id
  coarbitrary (AddF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (MulF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (ExpF f1 b) = coarbitrary f1 . coarbitrary b
  coarbitrary (SubF f1 f2) = coarbitrary f1 . coarbitrary f2
  coarbitrary (SinF f1) = coarbitrary f1
