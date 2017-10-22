{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_the_project (
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

bindir     = "/home/cptrodolfox/.cabal/bin"
libdir     = "/home/cptrodolfox/.cabal/lib/x86_64-linux-ghc-8.0.2/the-project-0.1.0.0"
dynlibdir  = "/home/cptrodolfox/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/cptrodolfox/.cabal/share/x86_64-linux-ghc-8.0.2/the-project-0.1.0.0"
libexecdir = "/home/cptrodolfox/.cabal/libexec"
sysconfdir = "/home/cptrodolfox/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "the_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "the_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "the_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "the_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "the_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "the_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
