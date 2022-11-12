module Select where

import Data.List (findIndex)
import Data.Map (Map, fromList, toList)
import Types1 (Selected (..), TimeStep)

instance Functor Selected where
  fmap f s = s {val = f (val s)}

instance Eq a => Eq (Selected a) where
  s1 == s2 = val s1 == val s2

instance Ord a => Ord (Selected a) where
  compare s1 s2 = compare (val s1) (val s2)

instance Show a => Show (Selected a) where
  show (Selected t v) = "Selected: time " ++ show t ++ " item " ++ show v
  show (NotSelected v) = "Not Selected: " ++ show v

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
deSelect c = NotSelected (val c)

select :: Selected a -> Selected a
select (Selected b a) = Selected b a
select (NotSelected a) = Selected 0 a

selectFirst :: [Selected a] -> [Selected a]
selectFirst [] = []
selectFirst (c : cs) = select c : map deSelect cs

selectLast :: [Selected a] -> [Selected a]
selectLast = reverse . selectFirst . reverse

sTime :: TimeStep -> [Selected a] -> [Selected a]
sTime s ((Selected t d) : cs) = Selected (t + s) d : sTime s cs
sTime s ((NotSelected d) : cs) = NotSelected d : sTime s cs
sTime _ [] = []

getSingleSelected :: [Selected a] -> Maybe a
getSingleSelected ((Selected _ d) : cs) = Just d
getSingleSelected (_ : cs) = getSingleSelected cs
getSingleSelected [] = Nothing

getAllSelected :: [Selected a] -> [a]
getAllSelected ((Selected _ d) : cs) = d : getAllSelected cs
getAllSelected (_ : cs) = getAllSelected cs
getAllSelected [] = []

getSelectedIndex :: [Selected a] -> Maybe Int
getSelectedIndex = findIndex p
  where
    p (Selected _ _) = True
    p (NotSelected _) = False

smap :: (a -> a) -> [Selected a] -> [Selected a]
smap f [] = []
smap f ((Selected t d) : cs) = Selected t (f d) : smap f cs
smap f (c : cs) = c : smap f cs

popSelected :: [Selected a] -> [Selected a]
popSelected [] = []
popSelected ((Selected t d) : cs) = popSelected cs
popSelected (c : cs) = c : popSelected cs