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
import EIO
import Compiler(haskell,HaskellFlags(..))

data TestResult = Passed
                | CompilerFailed T.Text
                | OutputDiff FilePath FilePath [(FilePath,FileDiff)]

data TestCaseRunning = RunningCompiler | CheckingOutput                  

data TestADLCompiler = TestADLCompiler {
  tc_inputPath :: FilePath,
  tc_module :: FilePath,
  tc_expectedOutput :: FilePath
  } deriving (Typeable)

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
        result <- liftIO $ diffTree (tc_expectedOutput tc) tempDir
        case result of
          [] -> do
            liftIO $ removeDirectoryRecursive tempDir
            return Passed
          diffs -> return (OutputDiff (tc_expectedOutput tc) tempDir diffs)

testADLCompiler :: String -> FilePath -> FilePath -> FilePath -> Test
testADLCompiler name ipath mpath epath = Test name (TestADLCompiler ipath mpath epath)
  
main :: IO ()
main = defaultMain tests

tests =
  [ testADLCompiler "1. empty module" "test1/input" "test1/input/test.adl" "test1/output"
  , testADLCompiler "2. structs" "test2/input" "test2/input/test.adl" "test2/output"
  ]
