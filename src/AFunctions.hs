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

fromStringVar :: FunctionString -> Float -> AFunction
fromStringVar t n = fromString $ concat (m t)
  where
    p = (== 'x')
    m s = case dropWhile p s of
      "" -> []
      s' -> w : show n : m s''
        where
          (w, s'') = break p s'

fromString :: FunctionString -> AFunction
fromString "e" = Etime
fromString ('s' : 'i' : 'n' : '(' : cs) = SinF (fromString $ init cs)
fromString s
  | all (\p -> isNumber p || p == '.' || p == 'e') s = C (read s)
  | otherwise = case betweenParens s of
      "*" -> MulF fps sps
      "**" -> ExpF fps sps
      "+" -> AddF fps sps
      "-" -> SubF fps sps
      a -> error ("Could not match: " ++ a ++ " are you missing brackets?")
  where
    fps = fromString (firstParenSeg s)
    sps = fromString (lastParenSeg s)
