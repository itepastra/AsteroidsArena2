{-# LANGUAGE GADTs #-}
module AFunctions (AFunction (..), createFunc, simplify, collapse, toString, fromStringVar, fromString, getCarr) where

import Control.Applicative (Alternative, (<|>))
import Control.Monad ((<=<))
import Data.Char (isNumber)
import Data.Monoid (Sum (Sum))
import Data.Tuple (swap)
import ParenthesesHelpers (beforeParens, betweenParens, firstParenSeg, lastParenSeg)
import Safe (readMay)
import Types1 (ElapsedTime, FunctionString)
import Data.Functor.Classes (Eq1 (..))

data AFunction a where
  AddF :: AFunction a -> AFunction a -> AFunction a
  SubF :: AFunction a -> AFunction a -> AFunction a
  AbsF :: AFunction a -> AFunction a
  SigF :: AFunction a -> AFunction a
  MulF :: AFunction a -> AFunction a -> AFunction a
  DivF :: AFunction a -> AFunction a -> AFunction a
  ExpF :: AFunction a -> AFunction a
  LogF :: AFunction a -> AFunction a
  SinF :: AFunction a -> AFunction a
  CosF :: AFunction a -> AFunction a
  AsinF :: AFunction a -> AFunction a
  AcosF :: AFunction a -> AFunction a
  AtanF :: AFunction a -> AFunction a
  AsinhF :: AFunction a -> AFunction a
  AcoshF :: AFunction a -> AFunction a
  AtanhF :: AFunction a -> AFunction a
  C :: a -> AFunction a
  Var :: AFunction a

instance Eq a => Eq (AFunction a) where
  (C x) == (C y) = x == y
  Var == Var = True
  (MulF f1 f2) == (MulF f3 f4) = f1 == f3 && f2 == f4
  (AddF f1 f2) == (AddF f3 f4) = f1 == f3 && f2 == f4
  (SubF f1 f2) == (SubF f3 f4) = f1 == f3 && f2 == f4
  (DivF f1 f2) == (DivF f3 f4) = f1 == f2 && f3 == f4
  (ExpF f1) == (ExpF f2) = f1 == f2
  (AbsF f1) == (AbsF f2) = f1 == f2
  (SigF f1) == (SigF f2) = f1 == f2
  (SinF f1) == (SinF f2) = f1 == f2
  (LogF f1) == (LogF f2) = f1 == f2
  (CosF f1) == (CosF f2) = f1 == f2
  (AsinF f1) == (AsinF f2) = f1 == f2
  (AcosF f1) == (AcosF f2) = f1 == f2
  (AtanF f1) == (AtanF f2) = f1 == f2
  (AtanhF f1) == (AtanhF f2) = f1 == f2
  (AcoshF f1) == (AcoshF f2) = f1 == f2
  (AsinhF f1) == (AsinhF f2) = f1 == f2
  _ == _ = False


instance Num a => Num (AFunction a) where
  (+) = AddF
  (*) = MulF
  abs = AbsF
  signum = SigF
  fromInteger = C . fromInteger
  (-) = SubF

instance Fractional a => Fractional (AFunction a) where
  fromRational = C . fromRational
  (/) = DivF

instance (Ord a, Real a, Floating a) => Ord (AFunction a) where
  f1 <= f2 = createFunc f1 0 <= createFunc f2 0

instance (Real a, Floating a) => Real (AFunction a) where
  toRational = toRational . flip createFunc 0

instance Functor AFunction where
  fmap f (AddF x y) = AddF (fmap f x) (fmap f y)
  fmap f (SubF x y) = SubF (fmap f x) (fmap f y)
  fmap f (MulF x y) = MulF (fmap f x) (fmap f y)
  fmap f (DivF x y) = DivF (fmap f x) (fmap f y)
  fmap f (AbsF fa) = AbsF (fmap f fa)
  fmap f (SigF fa) = SigF (fmap f fa)
  fmap f (ExpF fa) = ExpF (fmap f fa)
  fmap f (SinF fa) = SinF (fmap f fa)
  fmap f (LogF fa) = LogF (fmap f fa)
  fmap f (CosF fa) = CosF (fmap f fa)
  fmap f (AsinF fa) = AsinF (fmap f fa)
  fmap f (AcosF fa) = AsinF (fmap f fa)
  fmap f (AtanF fa) = AsinF (fmap f fa)
  fmap f (AsinhF fa) = AsinhF (fmap f fa)
  fmap f (AcoshF fa) = AsinhF (fmap f fa)
  fmap f (AtanhF fa) = AsinhF (fmap f fa)
  fmap f (C fa) = C (f fa)
  fmap f Var = Var

instance Foldable AFunction where
  foldMap f Var = mempty
  foldMap f (C x) = f x
  foldMap f (MulF fa fb) = foldMap f fa <> foldMap f fb
  foldMap f (AddF fa fb) = foldMap f fa <> foldMap f fb
  foldMap f (SubF fa fb) = foldMap f fa <> foldMap f fb
  foldMap f (DivF fa fb) = foldMap f fa <> foldMap f fb
  foldMap f (ExpF fa) = foldMap f fa
  foldMap f (LogF fa) = foldMap f fa
  foldMap f (SigF fa) = foldMap f fa
  foldMap f (AbsF fa) = foldMap f fa
  foldMap f (SinF fa) = foldMap f fa
  foldMap f (CosF fa) = foldMap f fa
  foldMap f (AsinF fa) = foldMap f fa
  foldMap f (AcosF fa) = foldMap f fa
  foldMap f (AtanF fa) = foldMap f fa
  foldMap f (AsinhF fa) = foldMap f fa
  foldMap f (AcoshF fa) = foldMap f fa
  foldMap f (AtanhF fa) = foldMap f fa

instance Floating a => Floating (AFunction a) where
  pi = C pi
  exp = ExpF
  log = LogF
  sin = SinF
  cos = CosF
  asin = AsinF
  acos = AcosF
  atan = AtanF
  sinh = AsinhF
  cosh = AcoshF
  asinh = AsinhF
  acosh = AcoshF
  atanh = AtanhF

getCarr :: AFunction a -> [a]
getCarr = foldMap (: [])

createFunc :: Floating a => AFunction a -> a -> a
-- for all a
createFunc (C v) _ = v
createFunc Var x = x
-- for Num a
createFunc (MulF fa fb) et = createFunc fa et * createFunc fb et
createFunc (AddF fa fb) et = createFunc fa et + createFunc fb et
createFunc (SubF fa fb) et = createFunc fa et - createFunc fb et
createFunc (SigF fa) et = signum (createFunc fa et)
createFunc (AbsF fa) et = abs (createFunc fa et)
-- for Fractional a
createFunc (DivF fa fb) et = createFunc fa et / createFunc fb et
-- for Floating a
createFunc (ExpF fa) et = exp (createFunc fa et)
createFunc (LogF fa) et = log (createFunc fa et)
createFunc (SinF fa) et = sin (createFunc fa et)
createFunc (CosF fa) et = cos (createFunc fa et)
createFunc (AsinF fa) et = asin (createFunc fa et)
createFunc (AcosF fa) et = acos (createFunc fa et)
createFunc (AtanF fa) et = atan (createFunc fa et)
createFunc (AsinhF fa) et = sinh (createFunc fa et)
createFunc (AcoshF fa) et = cosh (createFunc fa et)
createFunc (AtanhF fa) et = tanh (createFunc fa et)

simplify :: (Floating a, Eq a) => AFunction a -> AFunction a
-- simple cases
simplify (AddF (C x) (C y)) = C (x + y)
simplify (MulF (C x) (C y)) = C (x * y)
simplify (SubF (C x) (C y)) = C (x - y)
simplify (ExpF (C x)) = C (exp x)
simplify (SinF (C x)) = C (sin x)
simplify (AbsF (C x)) = C (abs x)
simplify (SigF (C x)) = C (signum x)
-- cases where 1 side is simple
simplify (ExpF (LogF f2)) = f2
simplify (LogF (ExpF f2)) = f2
simplify (AddF (C x) (AddF (C y) f2)) = C (x + y) + f2
simplify (MulF (C x) (MulF (C y) f2)) = C (x * y) * f2
simplify (SubF (C x) (SubF (C y) f2)) = C (x - y) - f2
simplify (AddF (AddF (C y) f2) (C x)) = C (x + y) + f2
simplify (MulF (MulF (C y) f2) (C x)) = C (x * y) * f2
simplify (SubF (SubF (C y) f2) (C x)) = C (y - x) - f2
simplify (AbsF (AbsF f1)) = abs f1
simplify (SigF (SigF f1)) = signum f1
-- not directly simplifyable
simplify (AddF f1 f2)
  | f2 == 0 = f1
  | f1 == 0 = f2
  | f1 == f2 = 2 * f1
  | otherwise = simplify f1 + simplify f2
simplify (MulF f1 f2)
  | f1 == 0 || f2 == 0 = 0
  | f1 == 1 = f2
  | f2 == 1 = f1
  | otherwise = simplify f1 * simplify f2
simplify (SubF f1 f2)
  | f1 == f2 = 0
  | f2 == 0 = f1
  | otherwise = simplify f1 - simplify f2
simplify (DivF f1 f2)
  | f1 == f2 = 1
  | otherwise = simplify f1 / simplify f2
simplify (ExpF f1) = exp (simplify f1)
simplify (SinF f1) = sin (simplify f1)
simplify (AbsF f1) = abs (simplify f1)
simplify (SigF f1) = signum (simplify f1)
simplify (LogF f1) = log (simplify f1)
-- base units
simplify (C x) = C x
simplify Var = Var
simplify (CosF fa) = CosF fa
simplify (AsinF fa) = AsinF fa
simplify (AcosF fa) = AcosF fa
simplify (AtanF fa) = AtanF fa
simplify (AsinhF fa) = AsinhF fa
simplify (AcoshF fa) = AcoshF fa
simplify (AtanhF fa) = AtanhF fa

collapse :: (Eq a, Floating a) => AFunction a -> AFunction a
collapse f
  | sf == f = f
  | otherwise = collapse sf
  where
    sf = simplify f

instance Show a => Show (AFunction a) where
  show = toString

toString :: Show a => AFunction a -> FunctionString
toString (C v) = show v
toString Var = "e"
toString (MulF fa fb) = "(" ++ toString fa ++ ")*(" ++ toString fb ++ ")"
toString (DivF fa fb) = "(" ++ toString fa ++ ")/(" ++ toString fb ++ ")"
toString (AddF fa fb) = "(" ++ toString fa ++ ")+(" ++ toString fb ++ ")"
toString (SubF fa fb) = "(" ++ toString fa ++ ")-(" ++ toString fb ++ ")"
toString (ExpF fa) = "exp(" ++ toString fa ++ ")"
toString (SinF fa) = "sin(" ++ toString fa ++ ")"
toString (AbsF fa) = "abs(" ++ toString fa ++ ")"
toString (SigF fa) = "sig(" ++ toString fa ++ ")"
toString (LogF fa) = "log(" ++ toString fa ++ ")"
toString (CosF fa) = "cos(" ++ toString fa ++ ")"
toString (AsinF fa) = "asin(" ++ toString fa ++ ")"
toString (AcosF fa) = "acos(" ++ toString fa ++ ")"
toString (AtanF fa) = "atan(" ++ toString fa ++ ")"
toString (AsinhF fa) = "asinh(" ++ toString fa ++ ")"
toString (AcoshF fa) = "acosh(" ++ toString fa ++ ")"
toString (AtanhF fa) = "atanh(" ++ toString fa ++ ")"

fromStringVar :: (Read a, RealFrac a, Floating a, Show a) => FunctionString -> a -> Maybe (AFunction a)
fromStringVar t n = fromString $ concat (m t)
  where
    p = (== 'x')
    m s = case dropWhile p s of
      "" -> []
      s' -> w : show n : m s''
        where
          (w, s'') = break p s'

fromString :: (Read a, RealFrac a, Floating a) => FunctionString -> Maybe (AFunction a)
fromString = fs

fs :: (Read a, RealFrac a, Floating a) => String -> Maybe (AFunction a)
fs s = parseInfix s <|> parsePrefix s <|> parseExact s

parseExact :: String -> Maybe (AFunction a)
parseExact "e" = Just Var
parseExact s = Nothing

parsePrefix :: (Floating a, Read a, RealFrac a) => String -> Maybe (AFunction a)
parsePrefix str = case (beforeParens str, fromString $ init $ drop 4 str) of
  (Just "sin", Just fa) -> Just $ SinF fa
  (Just "abs", Just fa) -> Just $ abs fa
  (Just "sig", Just fa) -> Just $ signum fa
  (Just "exp", Just fa) -> Just $ exp fa
  (Just "log", Just fa) -> Just $ log fa
  _ -> fmap C (readMay str)

parseInfix :: (Read a, RealFrac a, Floating a) => String -> Maybe (AFunction a)
parseInfix s
  | any (\p -> p == '*' || p == '+' || p == '-' || p == '^' || p == '/') s =
      case (betweenParens s, fromString =<< firstParenSeg s, fromString =<< lastParenSeg s) of
        (Just "*", Just fps, Just sps) -> Just $ fps * sps
        (Just "+", Just fps, Just sps) -> Just $ fps + sps
        (Just "-", Just fps, Just sps) -> Just $ SubF fps sps
        (Just "/", Just fps, Just sps) -> Just $ fps / sps
        (_, _, _) -> Nothing
  | otherwise = Nothing
