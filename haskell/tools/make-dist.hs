#!/usr/bin/env cabal
{- cabal:
build-depends: base, temporary, unix, process, filepath, directory
-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad(when)
import Data.Char(isSpace,toLower)
import Data.Foldable(for_)
import Data.List(dropWhile, dropWhileEnd, isSuffixOf)
import Data.Monoid((<>))
import System.Directory(createDirectoryIfMissing,copyFile,getDirectoryContents, setCurrentDirectory)
import System.Environment (getEnv)
import System.FilePath((</>), normalise, takeDirectory)
import System.IO.Temp(withSystemTempDirectory)
import System.Posix.Files(isRegularFile, isDirectory, getFileStatus)
import System.Process(readCreateProcess, shell, callCommand)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

copyFiles :: FilePath -> FilePath -> (FilePath -> Bool) -> IO ()
copyFiles src target match = do
  createDirectoryIfMissing True target
  files <- getDirectoryContents src
  for_ files $ \file -> do
    status <- getFileStatus (src </> file)
    when (isRegularFile status && match file) $ do
      copyFile (src </> file) (target  </> file)
    when (isDirectory status && not (specialFile file)) $ do
      copyFiles (src </> file) (target  </> file) match

specialFile :: FilePath -> Bool
specialFile "." = True
specialFile ".." = True
specialFile _ = False

cabalCmd :: String -> IO String
cabalCmd args = trim <$> readCreateProcess (shell ("cabal " <> args)) ""

main = do
  repoRoot <- getEnv "REPO_ROOT"
  setCurrentDirectory (repoRoot </> "haskell")
  let zipFile = repoRoot </> "dist" </> "adl-bindist" <> ".zip"

  withSystemTempDirectory "distXXXX" $ \zipDir -> do
    cabalCmd ("install adlc --install-method=copy --installdir=" <> (zipDir </> "bin"))

    let zipLib = zipDir </> "lib"
    copyFiles (repoRoot </> "adl/stdlib") (zipLib </> "adl") (const True)

    putStrLn ("creating " <> zipFile)
    createDirectoryIfMissing True (takeDirectory zipFile)
    callCommand ("(cd " <> zipDir <> "; zip -r -q " <> zipFile <> " *)")

    
