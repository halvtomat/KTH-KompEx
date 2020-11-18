{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_app (
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

bindir     = "/home/daniel/Documents/id2202-danielg8/solutions/module1/cigrid/.stack-work/install/x86_64-linux-tinfo6/22585ac11cb2005bd284c1033de38a816681d466088138d3843f0921a3e8ef6c/8.8.4/bin"
libdir     = "/home/daniel/Documents/id2202-danielg8/solutions/module1/cigrid/.stack-work/install/x86_64-linux-tinfo6/22585ac11cb2005bd284c1033de38a816681d466088138d3843f0921a3e8ef6c/8.8.4/lib/x86_64-linux-ghc-8.8.4/app-0.1.0.0-4GiPF8lOUpf5IOSlpVBOC8-cigrid"
dynlibdir  = "/home/daniel/Documents/id2202-danielg8/solutions/module1/cigrid/.stack-work/install/x86_64-linux-tinfo6/22585ac11cb2005bd284c1033de38a816681d466088138d3843f0921a3e8ef6c/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/daniel/Documents/id2202-danielg8/solutions/module1/cigrid/.stack-work/install/x86_64-linux-tinfo6/22585ac11cb2005bd284c1033de38a816681d466088138d3843f0921a3e8ef6c/8.8.4/share/x86_64-linux-ghc-8.8.4/app-0.1.0.0"
libexecdir = "/home/daniel/Documents/id2202-danielg8/solutions/module1/cigrid/.stack-work/install/x86_64-linux-tinfo6/22585ac11cb2005bd284c1033de38a816681d466088138d3843f0921a3e8ef6c/8.8.4/libexec/x86_64-linux-ghc-8.8.4/app-0.1.0.0"
sysconfdir = "/home/daniel/Documents/id2202-danielg8/solutions/module1/cigrid/.stack-work/install/x86_64-linux-tinfo6/22585ac11cb2005bd284c1033de38a816681d466088138d3843f0921a3e8ef6c/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "app_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "app_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "app_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "app_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "app_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "app_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
