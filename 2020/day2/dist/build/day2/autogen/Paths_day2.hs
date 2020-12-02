{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_day2 (
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

bindir     = "/home/lucas/.cabal/bin"
libdir     = "/home/lucas/.cabal/lib/x86_64-linux-ghc-8.6.5/day2-0.1.0.0-5Q6iKZNL7iJ77Dlv4MxxeH"
dynlibdir  = "/home/lucas/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/lucas/.cabal/share/x86_64-linux-ghc-8.6.5/day2-0.1.0.0"
libexecdir = "/home/lucas/.cabal/libexec/x86_64-linux-ghc-8.6.5/day2-0.1.0.0"
sysconfdir = "/home/lucas/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "day2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "day2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "day2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "day2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "day2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "day2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
