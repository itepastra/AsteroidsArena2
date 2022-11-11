{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

data Var = X | Y | Z | I | J | K
  deriving (Eq, Bounded, Enum)

data SOp where
  SID :: SOp
  AbsF :: SOp
  SigF :: SOp
  --   Floating
  ExpF :: SOp
  LogF :: SOp
  --   trig
  SinF :: SOp
  CosF :: SOp
  ASinF :: SOp
  ACosF :: SOp
  ATanF :: SOp
  SinhF :: SOp
  CoshF :: SOp
  ASinhF :: SOp
  ACoshF :: SOp
  ATanhF :: SOp
  deriving (Eq)

data DOp where
  AddF :: DOp
  MulF :: DOp
  SubF :: DOp
  --   Fractional
  DivF :: DOp
  deriving (Eq)

data TOp where
  TID :: TOp
  deriving (Eq)

data VFunction b a where
  Variable :: b -> VFunction b a
  Constant :: a -> VFunction b a
  OneIn :: SOp -> VFunction b a -> VFunction b a
  TwoIn :: DOp -> VFunction b a -> VFunction b a -> VFunction b a
  ThreeIn :: TOp -> VFunction b a -> VFunction b a -> VFunction b a -> VFunction b a

instance Num a => Num (VFunction b a) where
  (+) = TwoIn AddF
  (*) = TwoIn MulF
  abs = OneIn AbsF
  signum = OneIn SigF
  fromInteger = Constant . fromInteger
  (-) = TwoIn SubF

instance Fractional a => Fractional (VFunction b a) where
  fromRational = Constant . fromRational
  (/) = TwoIn DivF

instance Floating a => Floating (VFunction b a) where
  pi = Constant pi
  exp = OneIn ExpF
  log = OneIn LogF
  sin = OneIn SinF
  cos = OneIn CosF
  asin = OneIn ASinF
  acos = OneIn ACosF
  atan = OneIn ATanF
  sinh = OneIn SinhF
  cosh = OneIn CoshF
  asinh = OneIn ASinhF
  acosh = OneIn ACoshF
  atanh = OneIn ATanhF

instance Functor (VFunction b) where
  fmap f (Constant a) = Constant (f a)
  fmap f (Variable x) = Variable x
  fmap f (OneIn op f1) = OneIn op (fmap f f1)
  fmap f (TwoIn op f1 f2) = TwoIn op (fmap f f1) (fmap f f2)
  fmap f (ThreeIn op f1 f2 f3) = ThreeIn op (fmap f f1) (fmap f f2) (fmap f f3)

instance (Eq a, Eq b) => Eq (VFunction b a) where
  Variable a == Variable b = a == b
  Constant a == Constant b = a == b
  (OneIn o1 a1) == (OneIn o2 a2) = o1 == o2 && a1 == a2
  (TwoIn o1 a1 b1) == (TwoIn o2 a2 b2) = o1 == o2 && a1 == a2 && b1 == b2
  (ThreeIn o1 a1 b1 c1) == (ThreeIn o2 a2 b2 c2) = o1 == o2 && a1 == a2 && b1 == b2 && c1 == c2
  _ == _ = False

-- func , z , x , y -> ans
mkNumFunc :: (Floating a, Bounded b, Eq b, Enum b) => VFunction b a -> a -> VFunction b a
mkNumFunc (Variable a) x = insertVar a x
mkNumFunc (Constant a) x = Constant a
mkNumFunc (OneIn op f1) x = createOneIn op $ mkNumFunc f1 x
mkNumFunc (TwoIn op f1 f2) x = createTwoIn op (mkNumFunc f1 x) (mkNumFunc f2 x)
mkNumFunc (ThreeIn op f1 f2 f3) x = createThreeIn op f1 f2 f3

insertVar :: (Bounded b, Eq b, Enum b) => b -> a -> VFunction b a
insertVar b a
  | b == minBound = Constant a
  | otherwise = Variable (pred b)

createOneIn :: (Floating a) => SOp -> a -> a
createOneIn SID = id
createOneIn AbsF = abs
createOneIn SigF = signum
createOneIn ExpF = exp
createOneIn LogF = log
createOneIn SinF = sin
createOneIn CosF = cos
createOneIn ASinF = asin
createOneIn ACosF = acos
createOneIn ATanF = atan
createOneIn SinhF = sinh
createOneIn CoshF = cosh
createOneIn ASinhF = asinh
createOneIn ACoshF = acosh
createOneIn ATanhF = atanh

createTwoIn :: (Floating a) => DOp -> a -> a -> a
createTwoIn AddF = (+)
createTwoIn MulF = (*)
createTwoIn SubF = (-)
createTwoIn DivF = (/)

createThreeIn = undefined
