module AFunctions (createFunc, AFunction (..), fromString, toString, collapse) where

import Data.Char (isNumber)
import Data.Tuple (swap)
import ParenthesesHelpers (betweenParens, firstParenSeg, lastParenSeg)
import Types1 (AFunction (..), ElapsedTime, FunctionString)

instance Eq AFunction where
  (C x) == (C y) = x == y
  Etime == Etime = True
  (MulF f1 f2) == (MulF f3 f4) = f1 == f3 && f2 == f4 || f1 == f4 && f2 == f3
  (AddF f1 f2) == (AddF f3 f4) = f1 == f3 && f2 == f4 || f1 == f4 && f2 == f3
  (ExpF f1 f2) == (ExpF f3 f4) = f1 == f3 && f2 == f4
  (SubF f1 f2) == (SubF f3 f4) = f1 == f3 && f2 == f4
  (SinF f1) == (SinF f2) = f1 == f2
  _ == _ = False

createFunc :: AFunction -> ElapsedTime -> Float
createFunc (C v) _ = v
createFunc Etime et = et
createFunc (MulF fa fb) et = createFunc fa et * createFunc fb et
createFunc (ExpF fa fb) et = createFunc fa et ** createFunc fb et
createFunc (AddF fa fb) et = createFunc fa et + createFunc fb et
createFunc (SubF fa fb) et = createFunc fa et - createFunc fb et
createFunc (SinF fa) et = sin (createFunc fa et)

simplify :: AFunction -> AFunction
-- simple cases
simplify (AddF (C x) (C y)) = C (x + y)
simplify (MulF (C x) (C y)) = C (x * y)
simplify (ExpF (C x) (C y)) = C (x ** y)
simplify (SubF (C x) (C y)) = C (x - y)
-- cases where 1 side is simple
simplify (AddF (C x) (AddF (C y) f2)) = AddF (C (x + y)) f2
simplify (MulF (C x) (MulF (C y) f2)) = MulF (C (x * y)) f2
simplify (SubF (C x) (SubF (C y) f2)) = AddF (C (x - y)) f2
simplify (AddF (AddF (C y) f2) (C x)) = AddF (C (x + y)) f2
simplify (MulF (MulF (C y) f2) (C x)) = MulF (C (x * y)) f2
simplify (SubF (SubF (C y) f2) (C x)) = SubF (C (y - x)) f2

-- not simplifyable
simplify a = a

collapse :: AFunction -> AFunction
collapse f
  | sf == f = f
  | otherwise = collapse sf
  where
    sf = simplify f

instance Show AFunction where
  show = toString

toString :: AFunction -> FunctionString
toString (C v) = show v
toString Etime = "e"
toString (MulF fa fb) = "(" ++ toString fa ++ ")*(" ++ toString fb ++ ")"
toString (AddF fa fb) = "(" ++ toString fa ++ ")+(" ++ toString fb ++ ")"
toString (SubF fa fb) = "(" ++ toString fa ++ ")-(" ++ toString fb ++ ")"
toString (ExpF fa fb) = "(" ++ toString fa ++ ")**(" ++ toString fb ++ ")"
toString (SinF fa) = "sin(" ++ toString fa ++ ")"

fromStringVar :: FunctionString -> Float -> Maybe AFunction
fromStringVar t n = fromString $ concat (m t)
  where
    p = (== 'x')
    m s = case dropWhile p s of
      "" -> []
      s' -> w : show n : m s''
        where
          (w, s'') = break p s'

fromString :: FunctionString -> Maybe AFunction
fromString [] = Nothing
fromString "e" = Just Etime
fromString ('s' : 'i' : 'n' : '(' : cs) = case fromString $ init cs of
  Just aaa -> Just $ SinF aaa
  Nothing -> Nothing
fromString s
  | all (\p -> isNumber p || p == '.' || p == 'e') s = Just $ C (read s)
  | otherwise = case (betweenParens s, fromString (firstParenSeg s), fromString (lastParenSeg s)) of
      ("*", Just fps, Just sps) -> Just $ MulF fps sps
      ("**", Just fps, Just sps) -> Just $ ExpF fps sps
      ("+", Just fps, Just sps) -> Just $ AddF fps sps
      ("-", Just fps, Just sps) -> Just $ SubF fps sps
      (_, Nothing, _) -> Nothing
      (_, _, Nothing) -> Nothing
      (_, _, _) -> Nothing
