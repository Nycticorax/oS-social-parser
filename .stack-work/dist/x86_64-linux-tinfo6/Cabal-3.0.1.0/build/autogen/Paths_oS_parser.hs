{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_oS_parser (
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

bindir     = "/home/hades/haskell/oS-parser/.stack-work/install/x86_64-linux-tinfo6/1be3219ec719f87b9c3f2fad2178706a8b8fcda266d5f5fd00df359db88013eb/8.8.4/bin"
libdir     = "/home/hades/haskell/oS-parser/.stack-work/install/x86_64-linux-tinfo6/1be3219ec719f87b9c3f2fad2178706a8b8fcda266d5f5fd00df359db88013eb/8.8.4/lib/x86_64-linux-ghc-8.8.4/oS-parser-0.1.0.0-KEpckzr8apZE1DsLOTQUsV"
dynlibdir  = "/home/hades/haskell/oS-parser/.stack-work/install/x86_64-linux-tinfo6/1be3219ec719f87b9c3f2fad2178706a8b8fcda266d5f5fd00df359db88013eb/8.8.4/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/hades/haskell/oS-parser/.stack-work/install/x86_64-linux-tinfo6/1be3219ec719f87b9c3f2fad2178706a8b8fcda266d5f5fd00df359db88013eb/8.8.4/share/x86_64-linux-ghc-8.8.4/oS-parser-0.1.0.0"
libexecdir = "/home/hades/haskell/oS-parser/.stack-work/install/x86_64-linux-tinfo6/1be3219ec719f87b9c3f2fad2178706a8b8fcda266d5f5fd00df359db88013eb/8.8.4/libexec/x86_64-linux-ghc-8.8.4/oS-parser-0.1.0.0"
sysconfdir = "/home/hades/haskell/oS-parser/.stack-work/install/x86_64-linux-tinfo6/1be3219ec719f87b9c3f2fad2178706a8b8fcda266d5f5fd00df359db88013eb/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "oS_parser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "oS_parser_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "oS_parser_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "oS_parser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "oS_parser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "oS_parser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
