{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module VFunctions where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Fixed (mod')
import Data.Map (Map, findWithDefault, fromList)
import GHC.Enum (boundedEnumFromThen)
import GHC.Generics (Generic)
import ParenthesesHelpers (beforeParens, betweenParens, firstParenSeg, lastParenSeg)
import Safe (readMay)
import Text.Read (readMaybe)

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
  deriving (Eq, Generic, FromJSON, ToJSON, Bounded, Enum)

data DOp where
  AddF :: DOp
  MulF :: DOp
  SubF :: DOp
  --   Fractional
  DivF :: DOp
  deriving (Eq, Generic, FromJSON, ToJSON, Bounded, Enum)

data TOp where
  TAddF :: TOp
  ClampF :: TOp
  deriving (Eq, Generic, FromJSON, ToJSON, Bounded, Enum)

data VFunction b a where
  Variable :: a -> VFunction b a
  Constant :: b -> VFunction b a
  OneIn :: SOp -> VFunction b a -> VFunction b a
  TwoIn :: DOp -> VFunction b a -> VFunction b a -> VFunction b a
  ThreeIn :: TOp -> VFunction b a -> VFunction b a -> VFunction b a -> VFunction b a

instance Num b => Num (VFunction b a) where
  (+) = TwoIn AddF
  (*) = TwoIn MulF
  abs = OneIn AbsF
  signum = OneIn SigF
  fromInteger = Constant . fromInteger
  (-) = TwoIn SubF

instance Fractional b => Fractional (VFunction b a) where
  fromRational = Constant . fromRational
  (/) = TwoIn DivF

instance Floating b => Floating (VFunction b a) where
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
  fmap f (Constant a) = Constant a
  fmap f (Variable x) = Variable (f x)
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

instance (Ord a, Ord b) => Ord (VFunction b a) where
  compare f1 f2 = compare (size f1) (size f2)

instance Foldable (VFunction b) where
  foldMap f (Variable a) = f a
  foldMap f (Constant a) = mempty
  foldMap f (OneIn _ f1) = foldMap f f1
  foldMap f (TwoIn _ f1 f2) = foldMap f f1 <> foldMap f f2
  foldMap f (ThreeIn _ f1 f2 f3) = foldMap f f1 <> foldMap f f2 <> foldMap f f3

instance Traversable (VFunction b) where
  traverse _ (Constant a) = pure (Constant a)
  traverse f (Variable a) = Variable <$> f a
  traverse f (OneIn op f1) = OneIn op <$> traverse f f1
  traverse f (TwoIn op f1 f2) = TwoIn op <$> traverse f f1 <*> traverse f f2
  traverse f (ThreeIn op f1 f2 f3) = ThreeIn op <$> traverse f f1 <*> traverse f f2 <*> traverse f f3

instance Applicative (VFunction a) where
  pure = Variable
  (<*>) = undefined

insert :: (a -> VFunction b a) -> VFunction b a -> VFunction b a
insert f (Constant a) = Constant a
insert f (Variable x) = f x
insert f (OneIn op f1) = OneIn op (insert f f1)
insert f (TwoIn op f1 f2) = TwoIn op (insert f f1) (insert f f2)
insert f (ThreeIn op f1 f2 f3) = ThreeIn op (insert f f1) (insert f f2) (insert f f3)

insertAt :: Eq a => a -> VFunction b a -> VFunction b a -> VFunction b a
insertAt v tf = insert (f v tf)
  where
    f a f b
      | a == b = f
      | otherwise = Variable b

simplify :: (Eq a, Eq b, Floating b) => VFunction b a -> VFunction b a
simplify (OneIn op f1) = simplifyOne op (simplify f1)
simplify (TwoIn op f1 f2) = simplifyTwo op (simplify f1) (simplify f2)
simplify (ThreeIn op f1 f2 f3) = simplifyThree op (simplify f1) (simplify f2) (simplify f3)
simplify a = a

collapse :: (Eq a, Eq b, Floating b) => VFunction b a -> VFunction b a
collapse f
  | f' == f = f
  | otherwise = collapse f'
  where
    f' = simplify f

size :: VFunction b a -> Word
size (Constant _) = 1
size (Variable _) = 1
size (OneIn _ f1) = 1 + size f1
size (TwoIn _ f1 f2) = 1 + size f1 + size f2
size (ThreeIn _ f1 f2 f3) = 1 + size f1 + size f2 + size f3

mkNumFunc :: (Ord a, Floating b, Ord b) => VFunction b a -> Map a b -> b
mkNumFunc (Variable a) x = insertVar a x
mkNumFunc (Constant a) x = a
mkNumFunc (OneIn op f1) x = createOneIn op (mkNumFunc f1 x)
mkNumFunc (TwoIn op f1 f2) x = createTwoIn op (mkNumFunc f1 x) (mkNumFunc f2 x)
mkNumFunc (ThreeIn op f1 f2 f3) x = createThreeIn op (mkNumFunc f1 x) (mkNumFunc f2 x) (mkNumFunc f3 x)

insertVar :: (Ord k, Num a) => k -> Map k a -> a
insertVar = findWithDefault 0

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

createThreeIn :: (Num a, Ord a) => TOp -> a -> a -> a -> a
createThreeIn TAddF x y z = x + y + z
createThreeIn ClampF x mi ma = min ma (max mi x)

simplifyOne :: Floating b => SOp -> VFunction b a -> VFunction b a
simplifyOne SID f1 = f1
simplifyOne AbsF (OneIn AbsF f1) = abs f1
simplifyOne AbsF f1 = abs f1
simplifyOne SigF (OneIn SigF f1) = signum f1
simplifyOne SigF f1 = signum f1
simplifyOne ExpF (OneIn LogF f1) = f1
simplifyOne ExpF f1 = exp f1
simplifyOne LogF (OneIn ExpF f1) = f1
simplifyOne LogF f1 = log f1
simplifyOne SinF f1 = sin f1
simplifyOne CosF f1 = cos f1
simplifyOne ASinF f1 = asin f1
simplifyOne ACosF f1 = acos f1
simplifyOne ATanF f1 = atan f1
simplifyOne SinhF f1 = sinh f1
simplifyOne CoshF f1 = cosh f1
simplifyOne ASinhF f1 = asinh f1
simplifyOne ACoshF f1 = acosh f1
simplifyOne ATanhF f1 = atanh f1

simplifyTwo :: (Fractional b, Eq a, Eq b) => DOp -> VFunction b a -> VFunction b a -> VFunction b a
simplifyTwo AddF (Constant a) (Constant b) = Constant (a + b)
simplifyTwo MulF (Constant a) (Constant b) = Constant (a * b)
simplifyTwo AddF f1 f2
  | f1 == Constant 0 = f2
  | f2 == Constant 0 = f1
  | f1 == f2 = 2 * f1
  | otherwise = f1 + f2
simplifyTwo MulF f1 f2
  | f2 == 1 = f1
  | f1 == 1 = f2
  | f1 == 0 || f2 == 0 = 0
  | otherwise = f1 * f2
simplifyTwo SubF f1 f2
  | f1 == f2 = 0
  | otherwise = f1 - f2
simplifyTwo DivF f1 f2
  | f1 == 0 = 0
  | f2 == 1 = f1
  | f2 == -1 = negate f1
  | otherwise = f1 / f2

simplifyThree :: Num b => TOp -> VFunction b a -> VFunction b a -> VFunction b a -> VFunction b a
simplifyThree TAddF f1 f2 f3 = f1 + f2 + f3
simplifyThree ClampF f1 f2 f3 = ThreeIn ClampF f1 f2 f3

instance (Show a, Show b) => Show (VFunction b a) where
  show (Constant v) = show v
  show (Variable v) = "$" ++ show v
  show (OneIn op fa) = toStringSingle op (show fa)
  show (TwoIn op fa fb) = toStringDouble op (show fa) (show fb)
  show (ThreeIn op fa fb fc) = undefined

instance Show SOp where
  show SID = "id"
  show AbsF = "abs"
  show SigF = "signum"
  show ExpF = "exp"
  show LogF = "log"
  show SinF = "sin"
  show ASinF = "asin"
  show SinhF = "sinh"
  show ASinhF = "asinh"
  show CosF = "cos"
  show ACosF = "acos"
  show CoshF = "cosh"
  show ACoshF = "acosh"
  show ATanF = "atan"
  show ATanhF = "atanh"

instance Show DOp where
  show AddF = "+"
  show MulF = "*"
  show SubF = "-"
  show DivF = "/"

toStringSingle :: SOp -> String -> String
toStringSingle op s = show op ++ "(" ++ s ++ ")"

toStringDouble :: DOp -> String -> String -> String
toStringDouble op s1 s2 = "(" ++ s1 ++ ")" ++ show op ++ "(" ++ s2 ++ ")"

fromString :: (Read a, Floating a, Read b) => String -> Maybe (VFunction a b)
fromString s = parseInfix s <|> parsePrefix s <|> parseVar s

parseVar :: (Read b, Read a) => String -> Maybe (VFunction b a)
parseVar ('$' : s) = case readMay s of
  Nothing -> Nothing
  Just any -> Just $ Variable any
parseVar s = case readMay s of
  Nothing -> Nothing
  Just any -> Just $ Constant any

parsePrefix :: (Read a, Read b, Floating a) => String -> Maybe (VFunction a b)
parsePrefix str = case (beforeParens str, fromString =<< firstParenSeg str) of
  (Just "abs", Just fa) -> Just $ abs fa
  (Just "signum", Just fa) -> Just $ signum fa
  (Just "exp", Just fa) -> Just $ exp fa
  (Just "log", Just fa) -> Just $ log fa
  (Just "sin", Just fa) -> Just $ sin fa
  (Just "cos", Just fa) -> Just $ cos fa
  (Just "sinh", Just fa) -> Just $ sinh fa
  (Just "cosh", Just fa) -> Just $ cosh fa
  (Just "asin", Just fa) -> Just $ asin fa
  (Just "acos", Just fa) -> Just $ acos fa
  (Just "asinh", Just fa) -> Just $ asinh fa
  (Just "acosh", Just fa) -> Just $ acosh fa
  (Just "atan", Just fa) -> Just $ atan fa
  (Just "atanh", Just fa) -> Just $ atanh fa
  (Just "id", Just fa) -> Just $ OneIn SID fa
  _ -> Nothing

parseInfix :: (Read a, Floating a, Read b) => String -> Maybe (VFunction a b)
parseInfix s
  | any (\p -> p == '*' || p == '+' || p == '-' || p == '^' || p == '/') s =
      case (betweenParens s, fromString =<< firstParenSeg s, fromString =<< lastParenSeg s) of
        (Just "*", Just fps, Just sps) -> Just $ fps * sps
        (Just "+", Just fps, Just sps) -> Just $ fps + sps
        (Just "-", Just fps, Just sps) -> Just $ fps - sps
        (Just "/", Just fps, Just sps) -> Just $ fps / sps
        (_, _, _) -> Nothing
  | otherwise = Nothing
