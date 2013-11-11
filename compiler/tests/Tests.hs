{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec

import System.FilePath(takeDirectory,(</>))
import System.Directory(getTemporaryDirectory,removeDirectoryRecursive,getCurrentDirectory)
import System.IO.Temp(createTempDirectory)

import ADL.Utils.FileDiff
import qualified Data.Text as T

import HaskellCustomTypes

import ADL.Compiler.EIO
import ADL.Compiler.Utils

import qualified ADL.Compiler.Backends.Verify as V
import qualified ADL.Compiler.Backends.Haskell as H
import qualified ADL.Compiler.Backends.Cpp as CPP

data CodeGenResult = MatchOutput
                   | CompilerFailed T.Text
                   | OutputDiff FilePath FilePath [(FilePath,FileDiff)]
   deriving (Eq,Show)                     

processCompilerOutput :: FilePath -> FilePath -> Either T.Text () -> IO CodeGenResult
processCompilerOutput _ tempDir (Left err) = do
  removeDirectoryRecursive tempDir
  return (CompilerFailed err)
processCompilerOutput epath tempDir (Right ()) = do
  diffs <- diffTree epath tempDir
  case diffs of
    [] -> do
      removeDirectoryRecursive tempDir
      return MatchOutput
    _ -> do
      cwd <- getCurrentDirectory
      return (OutputDiff (cwd </> epath) tempDir diffs)

runVerifyBackend :: FilePath -> [FilePath] -> IO CodeGenResult
runVerifyBackend ipath mpaths = do
  let flags =  V.VerifyFlags {
    V.vf_searchPath = [ipath]
    }
  er <- unEIO $ V.verify flags mpaths
  case er of
   (Left err) -> return (CompilerFailed err)
   (Right ()) -> return MatchOutput

runVerifyBackend0 :: FilePath -> IO CodeGenResult
runVerifyBackend0 mpath = runVerifyBackend (takeDirectory mpath) [mpath]

runHaskellBackend :: FilePath -> [FilePath] -> FilePath -> [FilePath] -> IO CodeGenResult
runHaskellBackend ipath mpaths epath customTypeFiles = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let flags =  H.HaskellFlags {
    H.hf_searchPath = [ipath],
    H.hf_modulePrefix = "ADL",
    H.hf_customTypeFiles = customTypeFiles,
    H.hf_fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
    }
  er <- unEIO $ H.generate flags getCustomTypes mpaths
  processCompilerOutput epath tempDir er

runHaskellBackend1 :: FilePath-> IO CodeGenResult
runHaskellBackend1 mpath = runHaskellBackend ipath [mpath] epath []
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "hs-output"

runCppBackend :: FilePath -> [FilePath] -> FilePath -> [FilePath] -> IO CodeGenResult
runCppBackend ipath mpaths epath customTypeFiles = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let flags = CPP.CppFlags {
    CPP.cf_searchPath = [ipath],
    CPP.cf_customTypeFiles = customTypeFiles,
    CPP.cf_fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
    }
  er <- unEIO $ CPP.generate flags mpaths
  processCompilerOutput epath tempDir er

runCppBackend1 :: FilePath-> IO CodeGenResult
runCppBackend1 mpath = runCppBackend ipath [mpath] epath []
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "cpp-output"

stdsrc :: FilePath
stdsrc = "../../runtime/adl"

stdfiles, stdHsCustomTypes, stdCppCustomTypes :: [FilePath]
stdfiles = ["../../runtime/adl/sys/types.adl", "../../runtime/adl/sys/rpc.adl", "../../runtime/adl/sys/sinkimpl.adl"]
stdHsCustomTypes = ["../../compiler/config/hs-custom-types.json"]
stdCppCustomTypes = ["../../compiler/config/cpp-custom-types.json"]

main :: IO ()
main = hspec $ do
  describe "adlc verify backend" $ do
    it "aborts with error for duplicate definitions of a name" $ do
      runVerifyBackend0 "test8/input/test.adl"
        `shouldReturn` (CompilerFailed "multiple definitions for X")
    it "aborts with error for inconsistent versioned/unversioned definitions of a name" $ do
      runVerifyBackend0 "test9/input/test.adl"
        `shouldReturn` (CompilerFailed "inconsistent version/unversioned definitions for X")
    it "succeeds for correctly numbered versions of a name" $ do
      runVerifyBackend0 "test10/input/test.adl"
        `shouldReturn` MatchOutput
    it "aborts with error for inconsistently numbered versions of a name" $ do
      runVerifyBackend0 "test11/input/test.adl"
        `shouldReturn` (CompilerFailed "inconsistent version numbers for X")
    
  describe "adlc haskell backend" $ do
    it "generates expected code for an empty module" $ do
      runHaskellBackend1 "test1/input/test.adl"
        `shouldReturn` MatchOutput
    it "generates expected code for various structures" $ do
      runHaskellBackend1 "test2/input/test.adl"
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      runHaskellBackend1 "test3/input/test.adl"
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      runHaskellBackend "test4/input" ["test4/input/test.adl"] "test4/hs-output" ["test4/input/hs-custom-types.json"]
          `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      runHaskellBackend1 "test5/input/test.adl"
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      runHaskellBackend stdsrc stdfiles "test6/hs-output" stdHsCustomTypes
          `shouldReturn` MatchOutput
    it "generates expected code type aliases and newtypes" $ do
      runHaskellBackend1 "test7/input/test.adl"
        `shouldReturn` MatchOutput

  describe "adlc cpp backend" $ do
    it "generates expected code for an empty module" $ do
      runCppBackend1 "test1/input/test.adl"
        `shouldReturn` MatchOutput
    it "generates expected code for various structures" $ do
      runCppBackend1 "test2/input/test.adl"
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      runCppBackend1 "test3/input/test.adl"
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      runCppBackend "test4/input" ["test4/input/test.adl"] "test4/cpp-output" ["test4/input/cpp-custom-types.json"]
          `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      runCppBackend1 "test5/input/test.adl"
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      runCppBackend stdsrc stdfiles "test6/cpp-output" stdCppCustomTypes
          `shouldReturn` MatchOutput
    it "generates expected code type aliases and newtypes" $ do
      runCppBackend1 "test7/input/test.adl"
        `shouldReturn` MatchOutput
