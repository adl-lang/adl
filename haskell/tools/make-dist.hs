#!/usr/bin/env stack
{- stack --install-ghc runghc -}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad(when)
import Data.Char(isSpace,toLower)
import Data.Foldable(for_)
import Data.List(dropWhile, dropWhileEnd, isSuffixOf)
import Data.Monoid((<>))
import System.Directory(createDirectoryIfMissing,copyFile,getDirectoryContents)
import System.FilePath((</>), takeDirectory)
import System.IO.Temp(withSystemTempDirectory)
import System.Posix.Files(isRegularFile,getFileStatus)
import System.Process(readCreateProcess, shell, callCommand)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

stackPath :: String -> IO String
stackPath name = trim <$> readCreateProcess (shell ("stack path --" <> name)) ""

copyFiles :: FilePath -> FilePath -> (FilePath -> Bool) -> IO ()
copyFiles src target match = do
  createDirectoryIfMissing True target
  files <- getDirectoryContents src
  for_ files $ \file -> do
    isFile <- isRegularFile <$> getFileStatus (src </> file)
    when (isFile && match file) $ do
      copyFile (src </> file) (target  </> file)

specialFile :: FilePath -> Bool
specialFile "." = True
specialFile ".." = True
specialFile _ = False

main = do
  repoRoot <- takeDirectory <$> stackPath "project-root"
  localInstallRoot <- stackPath "local-install-root"
  version <- trim <$> readCreateProcess (shell ("git describe")) ""
  platform <- (map toLower . trim) <$> readCreateProcess (shell ("uname")) ""
                                
  let zipFile = repoRoot </> "dist" </> "adl-" <> version <> "-" <> platform <> ".zip"

  withSystemTempDirectory "distXXXX" $ \zipDir -> do
    copyFiles (localInstallRoot </> "bin") (zipDir </> "bin") (=="adlc")

    let src =   repoRoot </> "adl/stdlib/sys"
        target = zipDir </> "lib/adl/sys"
    copyFiles src target (const True)

    let src =   repoRoot </> "haskell/compiler/lib/adl/adlc/config"
        target = zipDir </> "lib/adl/adlc/config"
    copyFiles src target (const True)

    let src =   repoRoot </> "java/runtime/src/main/java/org/adl/runtime"
        target = zipDir </> "lib/java/runtime/org/adl/runtime"
    copyFiles src target (isSuffixOf ".java")

    putStrLn ("creating " <> zipFile)
    createDirectoryIfMissing True (takeDirectory zipFile)
    callCommand ("(cd " <> zipDir <> "; zip -r -q " <> zipFile <> " *)")

    
