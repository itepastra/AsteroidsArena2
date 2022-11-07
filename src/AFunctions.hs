module AFunctions where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isNumber)
import Data.List (elemIndices)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import GHC.Read (Read (readPrec))
import Types1 (ElapsedTime, FunctionString)

data AFunction = MulF AFunction AFunction | ExpF AFunction AFunction | AddF AFunction AFunction | C Float | Etime | SinF AFunction | SubF AFunction AFunction

createFunc :: AFunction -> ElapsedTime -> Float
createFunc (C v) _ = v
createFunc Etime et = et
createFunc (MulF fa fb) et = createFunc fa et * createFunc fb et
createFunc (ExpF fa fb) et = createFunc fa et ** createFunc fb et
createFunc (AddF fa fb) et = createFunc fa et + createFunc fb et
createFunc (SubF fa fb) et = createFunc fa et - createFunc fb et
createFunc (SinF fa) et = sin (createFunc fa et)

test :: (Float -> AFunction) -> Float -> ElapsedTime -> Float
test a f = createFunc (a f)

instance Show AFunction where
  show = toString

toString :: AFunction -> FunctionString
toString (C v) = show v
toString Etime = "E"
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
fromString "E" = Etime
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

-- answer by Stefan Holdermans at
-- https://stackoverflow.com/questions/10243290/determining-matching-parenthesis-in-haskell
parenPairs :: String -> [(Int, Int)]
parenPairs = go 0 []
  where
    go _ [] [] = []
    go _ (_ : _) [] = error "unbalanced parentheses!"
    go j acc ('(' : cs) = go (j + 1) (j : acc) cs
    go j [] (')' : cs) = error "unbalanced parentheses!"
    go j (i : is) (')' : cs) = (i, j) : go (j + 1) is cs
    go j acc (c : cs) = go (j + 1) acc cs

-- answer by Stefan Holdermans at
-- https://stackoverflow.com/questions/10243290/determining-matching-parenthesis-in-haskell
firstParenSeg :: String -> String
firstParenSeg s = f s (minimum (parenPairs s))
  where
    f s (i, j) = take (j - i - 1) (drop (i + 1) s)

-- derived from the above
lastParenSeg :: String -> String
lastParenSeg s = f s (maximum (map swap $ parenPairs s))
  where
    f s (j, i) = take (j - i - 1) (drop (i + 1) s)

-- derived from the above 2
betweenParens :: String -> String
betweenParens s = f s (snd $ minimum (parenPairs s)) (snd $ maximum (map swap $ parenPairs s))
  where
    f s i j = take (j - i - 1) (drop (i + 1) s)