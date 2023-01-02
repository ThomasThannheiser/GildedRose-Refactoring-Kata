{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_gilded_rose (
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

bindir     = "C:\\cabal\\bin"
libdir     = "C:\\cabal\\x86_64-windows-ghc-8.6.5\\gilded-rose-0.1.0.0-inplace-gilded-rose"
dynlibdir  = "C:\\cabal\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\cabal\\x86_64-windows-ghc-8.6.5\\gilded-rose-0.1.0.0"
libexecdir = "C:\\cabal\\gilded-rose-0.1.0.0-inplace-gilded-rose\\x86_64-windows-ghc-8.6.5\\gilded-rose-0.1.0.0"
sysconfdir = "C:\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "gilded_rose_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "gilded_rose_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "gilded_rose_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "gilded_rose_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gilded_rose_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gilded_rose_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
