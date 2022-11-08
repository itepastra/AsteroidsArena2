module AFunctions (createFunc, AFunction (..), fromString, toString) where

import Data.Char (isNumber)
import Data.Tuple (swap)
import ParenthesesHelpers (betweenParens, firstParenSeg, lastParenSeg)
import Types1 (AFunction (..), ElapsedTime, FunctionString)

createFunc :: AFunction -> ElapsedTime -> Float
createFunc (C v) _ = v
createFunc Etime et = et
createFunc (MulF fa fb) et = createFunc fa et * createFunc fb et
createFunc (ExpF fa fb) et = createFunc fa et ** createFunc fb et
createFunc (AddF fa fb) et = createFunc fa et + createFunc fb et
createFunc (SubF fa fb) et = createFunc fa et - createFunc fb et
createFunc (SinF fa) et = sin (createFunc fa et)

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
