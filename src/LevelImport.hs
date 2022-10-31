module LevelImport where

import Data.Aeson (decodeFileStrict, decodeFileStrict')
import JSONfuncs ()
import Level (InitLevelConfig)
import Model (Level)
import System.Directory.Tree (AnchoredDirTree ((:/)), DirTree (..), readDirectoryWith)

testie :: IO (AnchoredDirTree (Maybe Level))
testie = readDirectoryWith decodeFileStrict' "levels"

extractFile :: DirTree String -> Maybe Level
extractFile (File {file = f}) = Nothing
extractFile _ = Nothing
