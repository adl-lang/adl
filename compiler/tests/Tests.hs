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
import ADL.Compiler.Utils
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
  show (OutputDiff epath apath diffs) = "expected/actual:" ++ epath ++ " " ++ apath

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
    cwd <- liftIO $ getCurrentDirectory
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
          diffs -> return (OutputDiff (cwd </> (tb_expectedOutput tb)) tempDir diffs)

testHsBackend :: String -> FilePath -> [FilePath] -> FilePath -> [FilePath] -> Test
testHsBackend name ipath mpaths epath customTypeFiles = Test name (TestBackend epath run)
  where
    run tempDir = H.generate flags getCustomTypes mpaths
      where
        flags = H.HaskellFlags {
          H.hf_searchPath = [ipath],
          H.hf_modulePrefix = "ADL",
          H.hf_customTypeFiles = customTypeFiles,
          H.hf_fileWriter = writeOutputFile (OutputArgs (\s-> return ()) False tempDir)
          }

testCppBackend :: String -> FilePath -> [FilePath] -> FilePath -> [FilePath] -> Test
testCppBackend name ipath mpaths epath customTypeFiles = Test name (TestBackend epath run)
  where
    run tempDir = CPP.generate flags mpaths
      where
        flags = CPP.CppFlags {
          CPP.cf_searchPath = [ipath],
          CPP.cf_customTypeFiles = customTypeFiles,
          CPP.cf_fileWriter = writeOutputFile (OutputArgs (\s-> return ()) False tempDir)
          }

main :: IO ()
main = defaultMain tests

stdsrc = "../../runtime/adl"
stdfiles = ["../../runtime/adl/sys/types.adl", "../../runtime/adl/sys/rpc.adl"]

tests =
  [ testHsBackend "hs.1 empty module" "test1/input" ["test1/input/test.adl"] "test1/hs-output" []
  , testHsBackend "hs.2 structs" "test2/input" ["test2/input/test.adl"] "test2/hs-output" []
  , testHsBackend "hs.3 structs - default overrides" "test3/input" ["test3/input/test.adl"] "test3/hs-output" []
  , testHsBackend "hs.4 custom type mappings" "test4/input" ["test4/input/test.adl"] "test4/hs-output" ["test4/input/hs-custom-types.json"]
  , testHsBackend "hs.5 unions" "test5/input" ["test5/input/test.adl"] "test5/hs-output" []
  , testHsBackend "hs.6 std library" stdsrc stdfiles "test6/hs-output" ["../../compiler/config/hs-custom-types.json"]
  , testHsBackend "hs.7 type aliases and newtypes" "test7/input" ["test7/input/test.adl"] "test7/hs-output" []

  , testCppBackend "cpp.1 empty module" "test1/input" ["test1/input/test.adl"] "test1/cpp-output" []
  , testCppBackend "cpp.2 structs" "test2/input" ["test2/input/test.adl"] "test2/cpp-output" []
  , testCppBackend "cpp.3 structs - default overrides" "test3/input" ["test3/input/test.adl"] "test3/cpp-output" []
  , testCppBackend "cpp.4 custom type mappings" "test4/input" ["test4/input/test.adl"] "test4/cpp-output" ["test4/input/cpp-custom-types.json"]
  , testCppBackend "cpp.5 unions" "test5/input" ["test5/input/test.adl"] "test5/cpp-output" []
  , testCppBackend "cpp.6 std library" stdsrc stdfiles "test6/cpp-output" ["../../compiler/config/cpp-custom-types.json"]
  , testCppBackend "cpp.7 type aliases and newtypes" "test7/input" ["test7/input/test.adl"] "test7/cpp-output" []
  ]
