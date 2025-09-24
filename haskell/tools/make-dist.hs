#!/usr/bin/env cabal
{- cabal:
build-depends: base, temporary, unix, process, filepath, directory
-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad(when)
import Data.Char(isSpace,toLower)
import Data.Foldable(for_)
import Data.List(dropWhile, dropWhileEnd, isSuffixOf, stripPrefix)
import Data.Maybe(catMaybes)
import Data.Monoid((<>))
import System.Directory(createDirectoryIfMissing,copyFile,getDirectoryContents, setCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath((</>), normalise, takeDirectory)
import System.IO.Temp(withSystemTempDirectory)
import System.Posix.Files(isRegularFile, isDirectory, getFileStatus)
import System.Process(readCreateProcess, shell, callCommand)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

cabalCmd :: String -> IO String
cabalCmd args = trim <$> readCreateProcess (shell ("cabal " <> args)) ""

main = do
  repoRoot <- getEnv "REPO_ROOT"
  setCurrentDirectory (repoRoot </> "haskell")
  let zipFile = repoRoot </> "dist" </> "adl-bindist" <> ".zip"

  withSystemTempDirectory "distXXXX" $ \zipDir -> do
    cabalCmd ("install adlc --install-method=copy --installdir=" <> (zipDir </> "bin"))

    srcFiles <- lines <$> cabalCmd "sdist adl-compiler --list-only"
    let srcLib = "./compiler/lib/" 
        libFiles = (catMaybes . map (stripPrefix srcLib)) srcFiles
        zipLib = zipDir </> "lib"

    for_ libFiles $ \f -> do
      let src = srcLib </> f
          target = zipLib </> f
          targetDir = takeDirectory target
      createDirectoryIfMissing True targetDir
      copyFile src target
      
    putStrLn ("creating " <> zipFile)
    createDirectoryIfMissing True (takeDirectory zipFile)
    callCommand ("(cd " <> zipDir <> "; zip -r -q " <> zipFile <> " *)")

    
