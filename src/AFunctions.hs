module AFunctions (AFunction (..), createFunc, simplify, collapse, toString, fromStringVar, fromString, getCarr) where

import Data.Char (isNumber)
import Data.Tuple (swap)
import ParenthesesHelpers (betweenParens, firstParenSeg, lastParenSeg)
import Types1 (AFunction (..), ElapsedTime, FunctionString)

instance Eq a => Eq (AFunction a) where
  (C x) == (C y) = x == y
  Etime == Etime = True
  (MulF f1 f2) == (MulF f3 f4) = f1 == f3 && f2 == f4 || f1 == f4 && f2 == f3
  (AddF f1 f2) == (AddF f3 f4) = f1 == f3 && f2 == f4 || f1 == f4 && f2 == f3
  (ExpF f1 b) == (ExpF f2 c) = f1 == f2 && b == c
  (SubF f1 f2) == (SubF f3 f4) = f1 == f3 && f2 == f4
  (SinF f1) == (SinF f2) = f1 == f2
  _ == _ = False

instance Semigroup (AFunction a) where
  (<>) = AddF

instance Num a => Monoid (AFunction a) where
  mempty = C 0

instance Num a => Num (AFunction a) where
  (+) = AddF
  (*) = MulF
  abs = AbsF
  signum = SigF
  fromInteger = C . fromInteger
  (-) = SubF

getCarr :: (Real a, Fractional a, Floating a) => AFunction a -> [a]
getCarr Etime = [0]
getCarr (C v) = [realToFrac v]
getCarr (MulF fa fb) = getCarr fa ++ getCarr fb
getCarr (AddF fa fb) = getCarr fa ++ getCarr fb
getCarr (SubF fa fb) = getCarr fa ++ getCarr fb
getCarr (ExpF fa b) = map (** realToFrac b) (getCarr fa)
getCarr (SigF fa) = getCarr fa
getCarr (AbsF fa) = getCarr fa
getCarr (SinF fa) = getCarr fa

createFunc :: (Real a) => AFunction a -> ElapsedTime -> Float
createFunc (C v) _ = realToFrac v
createFunc Etime et = et
createFunc (MulF fa fb) et = createFunc fa et * createFunc fb et
createFunc (AddF fa fb) et = createFunc fa et + createFunc fb et
createFunc (SubF fa fb) et = createFunc fa et - createFunc fb et
createFunc (ExpF fa b) et = createFunc fa et ** fromIntegral b
createFunc (SinF fa) et = sin (createFunc fa et)
createFunc (AbsF fa) et = abs (createFunc fa et)
createFunc (SigF fa) et = signum (createFunc fa et)

simplify :: (Floating a, Eq a) => AFunction a -> AFunction a
-- simple cases
simplify (AddF (C x) (C y)) = C (x + y)
simplify (MulF (C x) (C y)) = C (x * y)
simplify (SubF (C x) (C y)) = C (x - y)
simplify (ExpF (C x) b) = C (x ** realToFrac b)
simplify (SinF (C x)) = C (sin x)
-- cases where 1 side is simple
simplify (ExpF (ExpF f2 b) c) = ExpF f2 (b * c)
simplify (AddF (C x) (AddF (C y) f2)) = AddF (C (x + y)) f2
simplify (MulF (C x) (MulF (C y) f2)) = MulF (C (x * y)) f2
simplify (SubF (C x) (SubF (C y) f2)) = AddF (C (x - y)) f2
simplify (AddF (AddF (C y) f2) (C x)) = AddF (C (x + y)) f2
simplify (MulF (MulF (C y) f2) (C x)) = MulF (C (x * y)) f2
simplify (SubF (SubF (C y) f2) (C x)) = SubF (C (y - x)) f2
-- not directly simplifyable
simplify (AddF f1 f2)
  | f1 == f2 = MulF (C 2) f1
  | otherwise = AddF (simplify f1) (simplify f2)
simplify (MulF f1 f2)
  | f1 == f2 = ExpF f1 2
  | otherwise = MulF (simplify f1) (simplify f2)
simplify (SubF f1 f2)
  | f1 == f2 = C 0
  | otherwise = SubF (simplify f1) (simplify f2)
simplify (SinF f1) = SinF (simplify f1)
simplify (ExpF f1 b) = ExpF (simplify f1) b
simplify (AbsF f1) = AbsF (simplify f1)
simplify (SigF f1) = SigF (simplify f1)
-- base units
simplify (C x) = C x
simplify Etime = Etime

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
toString Etime = "e"
toString (MulF fa fb) = "(" ++ toString fa ++ ")*(" ++ toString fb ++ ")"
toString (AddF fa fb) = "(" ++ toString fa ++ ")+(" ++ toString fb ++ ")"
toString (SubF fa fb) = "(" ++ toString fa ++ ")-(" ++ toString fb ++ ")"
toString (ExpF fa b) = "(" ++ toString fa ++ ")^(" ++ show b ++ ")"
toString (SinF fa) = "sin(" ++ toString fa ++ ")"
toString (AbsF fa) = "abs(" ++ toString fa ++ ")"
toString (SigF fa) = "sig(" ++ toString fa ++ ")"

fromStringVar :: (Show a, Read a, RealFrac a) => FunctionString -> a -> Maybe (AFunction a)
fromStringVar t n = fromString $ concat (m t)
  where
    p = (== 'x')
    m s = case dropWhile p s of
      "" -> []
      s' -> w : show n : m s''
        where
          (w, s'') = break p s'

fromString :: (Read a, RealFrac a) => FunctionString -> Maybe (AFunction a)
fromString [] = Nothing
fromString "e" = Just Etime
fromString (('s' : 'i' : 'n' : '(' : cs)) = case fromString $ init cs of
  Just aaa -> Just $ SinF aaa
  Nothing -> Nothing
fromString (('a' : 'b' : 's' : '(' : cs)) = case fromString $ init cs of
  Just aaa -> Just $ AbsF aaa
  Nothing -> Nothing
fromString (('s' : 'i' : 'g' : '(' : cs)) = case fromString $ init cs of
  Just aaa -> Just $ SigF aaa
  Nothing -> Nothing
fromString s
  | all (\p -> isNumber p || p == '.' || p == 'e' || p == '-') s = Just $ C (read s)
  | otherwise = case (betweenParens s, fromString =<< firstParenSeg s, fromString =<< lastParenSeg s) of
      (Just "*", Just fps, Just sps) -> Just $ MulF fps sps
      (Just "+", Just fps, Just sps) -> Just $ AddF fps sps
      (Just "-", Just fps, Just sps) -> Just $ SubF fps sps
      (Just "^", Just fps, Just (C sps)) -> Just $ ExpF fps (round sps)
      (_, _, _) -> Nothing
