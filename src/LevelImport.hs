module LevelImport where

import Model (Level (Level), GameStateInit (GameStateInit))

import Level (LevelConfig(LevelConfig))


importLevel :: FilePath -> IO Level
importLevel fp = do
  contents <- readFile fp
  return emptyLvl


emptyLvl :: Level
emptyLvl = Level "" (GameStateInit [] (LevelConfig "id"))
