module ParenthesesHelpers where

import Data.Tuple (swap)
import Safe.Foldable (maximumMay, minimumMay)

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
firstParenSeg :: String -> Maybe String
firstParenSeg s = case minimumMay (parenPairs s) of
  Just m -> Just $ f s m
  Nothing -> Nothing
  where
    f s (i, j) = take (j - i - 1) (drop (i + 1) s)

-- derived from the above
lastParenSeg :: String -> Maybe String
lastParenSeg s = case maximumMay (map swap $ parenPairs s) of
  Just m -> Just $ f s m
  Nothing -> Nothing
  where
    f s (j, i) = take (j - i - 1) (drop (i + 1) s)

-- derived from the above 2
betweenParens :: String -> Maybe String
betweenParens s = case (minimumMay (parenPairs s), maximumMay (map swap $ parenPairs s)) of
  (Just (_, min), Just (_, max)) -> Just $ f s min max
  (_, _) -> Nothing
  where
    f s i j = take (j - i - 1) (drop (i + 1) s)

beforeParens :: String -> Maybe String
beforeParens s = case minimumMay (parenPairs s) of
  Nothing -> Nothing
  Just (a, _) -> Just $ take a s
