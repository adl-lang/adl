{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Main where

import Prelude hiding (catch)
import Control.Exception

import qualified Data.Text as T

import Test.Framework
import Test.Framework.Providers.API

import System.IO.Temp(createTempDirectory)
import System.Directory(getTemporaryDirectory,removeDirectoryRecursive)

import EIO
import Compiler(haskell,HaskellFlags(..))

data TestResult = Passed
                | CompilerFailed T.Text
                | MissingOutput FilePath
                | ExtraOutput FilePath
                | OutputDiff FilePath FilePath

data TestCaseRunning = RunningCompiler | CheckingOutput                  

data TestADLCompiler = TestADLCompiler {
  tc_inputPath :: FilePath,
  tc_module :: FilePath,
  tc_expectedOutput :: FilePath
  }

instance Show TestResult where
  show Passed = "OK"
  show (CompilerFailed err) = "adlc failed: " ++ T.unpack err
  show (MissingOutput fpath) = "adlc failed to create " ++ fpath
  show (ExtraOutput fpath) = "adlc generated unexpected file " ++ fpath
  show (OutputDiff efpath afpath) = "expected output " ++ efpath ++ ", adlc generated " ++ afpath

instance Show TestCaseRunning where
  show RunningCompiler = "Running Compiler"
  show CheckingOutput = "Checking Compiler Output"

instance TestResultlike TestCaseRunning TestResult where
  testSucceeded Passed = True
  testSucceeded _ = False

instance Testlike TestCaseRunning TestResult TestADLCompiler where
  testTypeName _ = "adlc test"
  runTest topts tc = runImprovingIO $ do
    tempDir <- liftIO $ do
      tdir <- getTemporaryDirectory
      createTempDirectory tdir "adl.test." 
    yieldImprovement RunningCompiler
    let hf = HaskellFlags {
          hf_searchPath = [tc_inputPath tc],
          hf_modulePrefix = "ADL",
          hf_outputPath = tempDir,
          hf_noOverwrite = False
          }
    e <- liftIO $ unEIO (haskell hf [tc_module tc])
    case e of
      (Left emsg) -> return (CompilerFailed emsg)
      (Right ()) -> do
        yieldImprovement CheckingOutput
        result <- liftIO $ diffTrees (tc_expectedOutput tc) tempDir
        case result of
          Passed -> liftIO $ removeDirectoryRecursive tempDir
          _ -> return ()
        return result

diffTrees :: FilePath -> FilePath -> IO TestResult
diffTrees expected actual = return Passed

testADLCompiler :: String -> FilePath -> FilePath -> FilePath -> Test
testADLCompiler name ipath mpath epath = Test name (TestADLCompiler ipath mpath epath)
  
main :: IO ()
main = defaultMain tests

tests =
  [
    testADLCompiler "test1 - simple" "test1/input" "test1/input/test.adl" "test1/output"
  ]
