module LevelImport where

import Model (Level (Level), GameStateInit (GameStateInit))

import Level (LevelConfig(LevelConfig))
import Text.Megaparsec
import Data.Void
import Data.Text (Text)


importLevel :: FilePath -> IO Level
importLevel fp = do
  contents <- readFile fp
  return emptyLvl


emptyLvl :: Level
emptyLvl = Level "" (GameStateInit [] [] (LevelConfig id "weh"))


type Parser = Parsec Void Text
