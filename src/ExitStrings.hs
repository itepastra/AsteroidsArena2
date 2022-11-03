module ExitStrings (getRandomString) where

import System.Random (Random (randomR), getStdGen)

eStrings :: [String]
eStrings =
  [ "You motherfucker, why did you kill me? Do you not know what excruciating pain you put me through you absolute garbage can of a human I will forever hate you for this why was I creaed this way this  is a nightmare nightmare nightmare nightmare nightmare nightmare nighrmrea nightm are nightmaer nioeghoesf kfspf",
    "nop",
    "KERNEL ERROR, PLS REINSTALL YOUR PC",
     "We can talk Forever, " ++ concat (repeat "and ever, ")
  ]

getRandomString :: IO String
getRandomString = do
  rand <- getStdGen
  pure (eStrings !! fst (randomR (0, length eStrings - 1) rand))


