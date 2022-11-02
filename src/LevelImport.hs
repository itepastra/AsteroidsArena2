module LevelImport where

import Data.Aeson (decodeFileStrict', encodeFile)
import Data.Maybe (mapMaybe)
import JSONfuncs ()
import Model (Level (name))
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.Directory.Tree (AnchoredDirTree (dirTree, (:/)), DirTree (..), flattenDir, readDirectoryWith)

fileLevels :: IO (AnchoredDirTree (Maybe Level))
fileLevels = readDirectoryWith decodeFileStrict' "levels"

cleanFileLevels :: IO [Level]
cleanFileLevels = filterLevels . dirTree <$> fileLevels

encodeLevels :: [Level] -> IO ()
encodeLevels lvls =
  do
    createDirectoryIfMissing True "levels/default"
    mapM_ (\x -> encodeFile ("levels/default/" ++ Model.name x ++ ".json") x) lvls

filterLevels :: DirTree (Maybe Level) -> [Level]
filterLevels = mapMaybe levelCheck . flattenDir

levelCheck :: DirTree (Maybe Level) -> Maybe Level
levelCheck (File {file = x}) = x
levelCheck _ = Nothing