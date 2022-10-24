module LevelImport where

import Model (Level (Level), GameStateInit (GameStateInit))
import Text.Parsec
import Text.Parsec.String (GenParser)
import Level (LevelConfig(LevelConfig))

importLevel :: FilePath -> IO Level
importLevel fp = do
  contents <- readFile fp
  return emptyLvl

--   return $ readLvl (lines contents)

emptyLvl :: Level
emptyLvl = Level "" (GameStateInit [] [] (LevelConfig id 5))

readLvl :: GenParser Char st Level
readLvl =
  do
    result <- many line
    eof
    return result

