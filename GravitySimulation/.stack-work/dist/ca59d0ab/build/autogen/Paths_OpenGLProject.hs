{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_OpenGLProject (
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

bindir     = "C:\\Users\\Henrik\\Documents\\Projekt\\Haskell\\OpenGLTest\\GravitySimulation\\.stack-work\\install\\ccbceaec\\bin"
libdir     = "C:\\Users\\Henrik\\Documents\\Projekt\\Haskell\\OpenGLTest\\GravitySimulation\\.stack-work\\install\\ccbceaec\\lib\\x86_64-windows-ghc-8.0.2\\OpenGLProject-0.1.0.0"
dynlibdir  = "C:\\Users\\Henrik\\Documents\\Projekt\\Haskell\\OpenGLTest\\GravitySimulation\\.stack-work\\install\\ccbceaec\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\Users\\Henrik\\Documents\\Projekt\\Haskell\\OpenGLTest\\GravitySimulation\\.stack-work\\install\\ccbceaec\\share\\x86_64-windows-ghc-8.0.2\\OpenGLProject-0.1.0.0"
libexecdir = "C:\\Users\\Henrik\\Documents\\Projekt\\Haskell\\OpenGLTest\\GravitySimulation\\.stack-work\\install\\ccbceaec\\libexec"
sysconfdir = "C:\\Users\\Henrik\\Documents\\Projekt\\Haskell\\OpenGLTest\\GravitySimulation\\.stack-work\\install\\ccbceaec\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "OpenGLProject_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "OpenGLProject_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "OpenGLProject_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "OpenGLProject_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "OpenGLProject_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "OpenGLProject_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
