{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, DeriveDataTypeable #-}
module Main where

import Prelude hiding (catch)
import Data.Typeable
import Control.Monad
import Control.Exception

import qualified Data.Text as T
import qualified Data.Set as Set

import Test.Framework
import Test.Framework.Providers.API

import System.IO.Temp(createTempDirectory)
import System.Directory
import System.FilePath

import ADL.Utils.FileDiff
import ADL.Compiler.EIO
import qualified ADL.Compiler.Backends.Haskell as H
import qualified ADL.Compiler.Backends.Cpp as CPP
import HaskellCustomTypes

data TestResult = Passed
                | CompilerFailed T.Text
                | OutputDiff FilePath FilePath [(FilePath,FileDiff)]

data TestCaseRunning = RunningCompiler | CheckingOutput                  

instance Show TestResult where
  show Passed = "OK"
  show (CompilerFailed err) = "adlc failed: " ++ T.unpack err
  show (OutputDiff epath apath diffs) = "expected:" ++ epath ++ ", actual:" ++ apath

instance Show TestCaseRunning where
  show RunningCompiler = "Running Compiler"
  show CheckingOutput = "Checking Compiler Output"

instance TestResultlike TestCaseRunning TestResult where
  testSucceeded Passed = True
  testSucceeded _ = False

data TestBackend = TestBackend {
  tb_expectedOutput :: FilePath,
  tb_run :: FilePath -> EIOT ()
  } deriving (Typeable)

instance Testlike TestCaseRunning TestResult TestBackend where
  testTypeName _ = "adlc test"
  runTest topts tb = runImprovingIO $ do
    tempDir <- liftIO $ do
      tdir <- getTemporaryDirectory
      createTempDirectory tdir "adl.test." 
    yieldImprovement RunningCompiler
    e <- liftIO $ unEIO (tb_run tb tempDir)
    case e of
      (Left emsg) -> return (CompilerFailed emsg)
      (Right ()) -> do
        yieldImprovement CheckingOutput
        result <- liftIO $ diffTree (tb_expectedOutput tb) tempDir
        case result of
          [] -> do
            liftIO $ removeDirectoryRecursive tempDir
            return Passed
          diffs -> return (OutputDiff (tb_expectedOutput tb) tempDir diffs)

testHsBackend :: String -> FilePath -> FilePath -> FilePath -> (H.HaskellFlags -> H.HaskellFlags) -> Test
testHsBackend name ipath mpath epath ff = Test name (TestBackend epath run)
  where
    run tempDir = H.generate (ff $ flags) getCustomTypes [mpath]
      where
        flags = H.HaskellFlags {
          H.hf_searchPath = [ipath],
          H.hf_modulePrefix = "ADL",
          H.hf_outputPath = tempDir,
          H.hf_customTypeFiles = [],
          H.hf_noOverwrite = False
          }

testCppBackend :: String -> FilePath -> FilePath -> FilePath -> (CPP.CppFlags -> CPP.CppFlags) -> Test
testCppBackend name ipath mpath epath ff = Test name (TestBackend epath run)
  where
    run tempDir = CPP.generate (ff $ flags) [mpath]
      where
        flags = CPP.CppFlags {
          CPP.cf_searchPath = [ipath],
          CPP.cf_outputPath = tempDir,
          CPP.cf_customTypeFiles = [],
          CPP.cf_noOverwrite = False
          }

main :: IO ()
main = defaultMain tests

tests =
  [ testHsBackend "hs.1 empty module" "test1/input" "test1/input/test.adl" "test1/hs-output" id
  , testHsBackend "hs.2 structs" "test2/input" "test2/input/test.adl" "test2/hs-output" id
  , testHsBackend "hs.3 structs - default overrides" "test3/input" "test3/input/test.adl" "test3/hs-output" id
  , testHsBackend "hs.4 custom type mappings" "test4/input" "test4/input/test.adl" "test4/output"
                      (\hf->hf{H.hf_customTypeFiles=["test4/input/custom_types.json"]})

  , testCppBackend "cpp.1 empty module" "test1/input" "test1/input/test.adl" "test1/cpp-output" id
  , testCppBackend "cpp.2 structs" "test2/input" "test2/input/test.adl" "test2/cpp-output" id
  , testCppBackend "cpp.3 structs - default overrides" "test3/input" "test3/input/test.adl" "test3/cpp-output" id
  , testCppBackend "cpp.5 unions" "test5/input" "test5/input/test.adl" "test5/cpp-output" id
  ]
