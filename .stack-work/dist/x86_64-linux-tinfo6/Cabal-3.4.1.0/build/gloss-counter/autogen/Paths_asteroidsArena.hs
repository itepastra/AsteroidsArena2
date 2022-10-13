{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_asteroidsArena (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/noa/programming/AsteroidsArena2/.stack-work/install/x86_64-linux-tinfo6/e52c5290215bf1a37d1729b673617e4bbea258166b00b336a44adabe271a3bfc/9.0.2/bin"
libdir     = "/home/noa/programming/AsteroidsArena2/.stack-work/install/x86_64-linux-tinfo6/e52c5290215bf1a37d1729b673617e4bbea258166b00b336a44adabe271a3bfc/9.0.2/lib/x86_64-linux-ghc-9.0.2/asteroidsArena-0.1.0.0-D3NvErHsvL69ncIJ2PcE3h-gloss-counter"
dynlibdir  = "/home/noa/programming/AsteroidsArena2/.stack-work/install/x86_64-linux-tinfo6/e52c5290215bf1a37d1729b673617e4bbea258166b00b336a44adabe271a3bfc/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/noa/programming/AsteroidsArena2/.stack-work/install/x86_64-linux-tinfo6/e52c5290215bf1a37d1729b673617e4bbea258166b00b336a44adabe271a3bfc/9.0.2/share/x86_64-linux-ghc-9.0.2/asteroidsArena-0.1.0.0"
libexecdir = "/home/noa/programming/AsteroidsArena2/.stack-work/install/x86_64-linux-tinfo6/e52c5290215bf1a37d1729b673617e4bbea258166b00b336a44adabe271a3bfc/9.0.2/libexec/x86_64-linux-ghc-9.0.2/asteroidsArena-0.1.0.0"
sysconfdir = "/home/noa/programming/AsteroidsArena2/.stack-work/install/x86_64-linux-tinfo6/e52c5290215bf1a37d1729b673617e4bbea258166b00b336a44adabe271a3bfc/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "asteroidsArena_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "asteroidsArena_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "asteroidsArena_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "asteroidsArena_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "asteroidsArena_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "asteroidsArena_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
