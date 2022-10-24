module LevelImport where
import Model (Level)

importLevel :: FilePath -> IO Level
importLevel fp = do  
        contents <- readFile fp
        return $ readLvl . lines $ contents 

readLvl :: [String] -> Level
readLvl s = undefined