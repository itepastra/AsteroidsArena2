module ParenthesesHelpers where
import Data.Tuple (swap)
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