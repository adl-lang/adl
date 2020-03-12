#!/usr/bin/env stack
{- stack --install-ghc runghc --package temporary -}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad(when)
import Data.Char(isSpace,toLower)
import Data.Foldable(for_)
import Data.List(dropWhile, dropWhileEnd, isSuffixOf)
import Data.Monoid((<>))
import System.Directory(createDirectoryIfMissing,copyFile,getDirectoryContents)
import System.FilePath((</>), takeDirectory)
import System.IO.Temp(withSystemTempDirectory)
import System.Posix.Files(isRegularFile, isDirectory, getFileStatus)
import System.Process(readCreateProcess, shell, callCommand)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

stackCmd :: String -> IO String
stackCmd args = trim <$> readCreateProcess (shell ("stack " <> args)) ""

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

main = do
  repoRoot <- takeDirectory <$> stackCmd "path --project-root"
  localInstallRoot <- stackCmd "path --local-install-root"
  version <- trim <$> readCreateProcess (shell ("git describe")) ""
  platform <- (map toLower . trim) <$> readCreateProcess (shell ("uname")) ""
                                
  let zipFile = repoRoot </> "dist" </> "adl-" <> version <> "-" <> platform <> ".zip"

  withSystemTempDirectory "distXXXX" $ \zipDir -> do
    copyFiles (localInstallRoot </> "bin") (zipDir </> "bin") (=="adlc")

    adllib <- stackCmd "run adlc -- show --adlstdlib"

    let src =  takeDirectory adllib
        target = zipDir </> "lib"
    copyFiles src target (const True)

    putStrLn ("creating " <> zipFile)
    createDirectoryIfMissing True (takeDirectory zipFile)
    callCommand ("(cd " <> zipDir <> "; zip -r -q " <> zipFile <> " *)")

    
