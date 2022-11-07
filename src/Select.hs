module Select where

import Data.List (findIndex)
import Types1 (Selected (..), TimeStep)

instance Functor Selected where
  fmap f s = s {val = f (val s)}

instance Eq a => Eq (Selected a) where
  s1 == s2 = val s1 == val s2

instance Ord a => Ord (Selected a) where
  compare s1 s2 = compare (val s1) (val s2)


selectNext :: [Selected a] -> [Selected a]
selectNext ((Selected _ a) : c : cs) = NotSelected a : select c : cs
selectNext [Selected b a] = [Selected b a]
selectNext (c@(NotSelected _) : cs) = c : selectNext cs
selectNext [] = []

selectPrev :: [Selected a] -> [Selected a]
selectPrev [a, Selected _ b] = [select a, NotSelected b]
selectPrev (a : (Selected _ b) : cs) = select a : NotSelected b : cs
selectPrev ((Selected t b) : cs) = Selected t b : cs
selectPrev (c : cs) = c : selectPrev cs
selectPrev [] = []

deSelect :: Selected a -> Selected a
deSelect (Selected _ a) = NotSelected a
deSelect (NotSelected a) = NotSelected a

select :: Selected a -> Selected a
select (Selected b a) = Selected b a
select (NotSelected a) = Selected 0 a

selectFirst :: [Selected a] -> [Selected a]
selectFirst [] = []
selectFirst (c : cs) = select c : cs

selectLast :: [Selected a] -> [Selected a]
selectLast = reverse . selectFirst . reverse

sTime :: TimeStep -> [Selected a] -> [Selected a]
sTime s ((Selected t d) : cs) = Selected (t + s) d : sTime s cs
sTime s ((NotSelected d) : cs) = NotSelected d : sTime s cs
sTime _ [] = []

getSelected :: [Selected a] -> Maybe a
getSelected ((Selected _ d) : cs) = Just d
getSelected (_ : cs) = getSelected cs
getSelected [] = Nothing

getSelectedIndex :: [Selected a] -> Maybe Int
getSelectedIndex = findIndex p
  where
    p (Selected _ _) = True
    p (NotSelected _) = False