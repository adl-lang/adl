{-# LANGUAGE OverloadedStrings #-}
module Main where
import Test.Hspec

import Data.Monoid
import System.FilePath(takeDirectory,(</>),combine)
import System.Directory(getTemporaryDirectory,removeDirectoryRecursive,getCurrentDirectory,setCurrentDirectory)
import System.IO.Temp(createTempDirectory)
import Control.Concurrent.STM.TVar(newTVar,readTVar,modifyTVar',TVar)
import Control.Concurrent.STM(atomically)


import ADL.Utils.FileDiff
import qualified Data.Text as T

import HaskellCustomTypes

import ADL.Compiler.EIO
import ADL.Compiler.Utils
import ADL.Compiler.Processing(AdlFlags(..),defaultAdlFlags)

import qualified ADL.Compiler.Backends.Verify as V
import qualified ADL.Compiler.Backends.Haskell as H
import qualified ADL.Compiler.Backends.Cpp as CPP
import qualified ADL.Compiler.Backends.AST as AST
import qualified ADL.Compiler.Backends.Java as J
import qualified ADL.Compiler.Backends.Javascript as JS
import qualified ADL.Compiler.Backends.Typescript as TS
import qualified ADL.Compiler.Backends.Rust as RS

data CodeGenResult = MatchOutput
                   | CompilerFailed T.Text
                   | OutputDiff FilePath FilePath [(FilePath,FileDiff)]
   deriving (Eq)

instance Show CodeGenResult where
  show MatchOutput = "matching output"
  show (CompilerFailed t) = "compiler failure: " ++ T.unpack t
  show (OutputDiff expected actual diffs ) = "diff " ++ actual ++ "/ " ++ expected ++ "/ (details: " ++ show diffs ++ ")"


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

runVerifyBackend :: [FilePath] -> [FilePath] -> IO CodeGenResult
runVerifyBackend ipath mpaths = do
  let af =  defaultAdlFlags{af_searchPath=ipath}
  er <- unEIO $ V.verify af mpaths
  case er of
   (Left err) -> return (CompilerFailed err)
   (Right ()) -> return MatchOutput

runVerifyBackend1 :: FilePath -> IO CodeGenResult
runVerifyBackend1 mpath = runVerifyBackend [stdsrc,takeDirectory mpath] [mpath]

runHaskellBackend :: [FilePath] -> [FilePath] -> FilePath -> IO CodeGenResult
runHaskellBackend ipaths mpaths epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af =  defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=["adl-hs"]}
      hf =  H.HaskellFlags
        { H.hf_modulePrefix = "ADL"
        , H.hf_includeRuntime = Nothing
        , H.hf_runtimePackage = "ADL.Core"
        }
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
  er <- unEIO $ H.generate af hf fileWriter getCustomType mpaths
  processCompilerOutput epath tempDir er

runHaskellBackend1 :: FilePath-> IO CodeGenResult
runHaskellBackend1 mpath = runHaskellBackend [ipath,stdsrc] [mpath] epath
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "hs-output"

runCppBackend :: [FilePath] -> [FilePath] -> FilePath -> FilePath -> IO CodeGenResult
runCppBackend ipaths mpaths epath iprefix = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af =  defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=["adl-cpp"]}
      cf = CPP.CppFlags {
        CPP.cf_incFilePrefix = iprefix,
        CPP.cf_includeRelops = True
        }
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
  er <- unEIO $ CPP.generate af cf fileWriter mpaths
  processCompilerOutput epath tempDir er

runCppBackend1 :: FilePath-> IO CodeGenResult
runCppBackend1 mpath = runCppBackend [ipath,stdsrc] [mpath] epath ""
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "cpp-output"

runAstBackend :: [FilePath] -> [FilePath] -> FilePath -> IO CodeGenResult
runAstBackend ipath mpaths epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af = defaultAdlFlags{af_searchPath=ipath,af_mergeFileExtensions=[".ann"]}
      bf = AST.AstFlags Nothing
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
  er <- unEIO $ AST.generate af bf fileWriter mpaths
  processCompilerOutput epath tempDir er

runAstBackend1 :: FilePath-> IO CodeGenResult
runAstBackend1 mpath = runAstBackend [ipath,stdsrc] [mpath] epath
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "ast-output"

runJavaBackend :: [FilePath] -> [FilePath] -> FilePath -> (J.JavaFlags -> J.JavaFlags) -> IO CodeGenResult
runJavaBackend ipaths mpaths epath updateflags = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af = defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=["adl-java"]}
      jf = J.JavaFlags {
        J.jf_libDir = "LIBDIR",
        J.jf_package = J.javaPackage "adl",
        J.jf_includeRuntime = False,
        J.jf_codeGenProfile = J.defaultCodeGenProfile {J.cgp_json=True}
        }
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
  er <- unEIO $ J.generate af (updateflags jf) fileWriter mpaths
  processCompilerOutput epath tempDir er

withJavaOutputPackage :: T.Text -> J.JavaFlags -> J.JavaFlags
withJavaOutputPackage package flags = flags{J.jf_package=J.javaPackage package}

runJavaBackend1 :: FilePath -> IO CodeGenResult
runJavaBackend1 mpath = runJavaBackend [ipath,stdsrc] [mpath] epath id
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "java-output"

runJsBackend :: [FilePath] -> [FilePath] -> FilePath -> IO CodeGenResult
runJsBackend ipaths mpaths epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af = defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=[]}
      jf = JS.JavascriptFlags {}
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
  er <- unEIO $ JS.generate af jf fileWriter mpaths
  processCompilerOutput epath tempDir er

runTsBackend :: [FilePath] -> [FilePath] -> FilePath -> IO CodeGenResult
runTsBackend ipaths mpaths epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adlt.test."
  let af = defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=[]}
      js = TS.TypescriptFlags {
        TS.tsIncludeRuntime=False,
        TS.tsIncludeResolver=True,
        TS.tsRuntimeDir="runtime",
        TS.tsLibDir="../../../haskell/compiler/lib",
        TS.tsExcludeAst=False,
        TS.tsExcludedAstAnnotations=Nothing
      }
      fileWriter = writeOutputFile (OutputArgs (\_ -> return ()) False tempDir)
  er <- unEIO $ TS.generate af js fileWriter mpaths
  processCompilerOutput epath tempDir er

runTsBackend1 mpath = runTsBackend [ipath,stdsrc] [mpath] epath
  where
    ipath = takeDirectory mpath
    epath = takeDirectory ipath </> "ts-output"

runRsBackend :: [FilePath] -> [FilePath] -> FilePath -> IO CodeGenResult
runRsBackend ipaths mpaths epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adlt.test."
  let af = defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=["adl-rs"]}
      js = RS.RustFlags {
        RS.rsModule = RS.rustModule "adl",
        RS.rsRuntimeModule = RS.rustModule "crate::adlrt"
      }
      fileWriter = writeOutputFile (OutputArgs (\_ -> return ()) False tempDir)
  er <- unEIO $ RS.generate af js fileWriter mpaths
  processCompilerOutput epath tempDir er

stdsrc :: FilePath
stdsrc = "../../../haskell/compiler/lib/adl"

stdfiles, stdHsCustomTypes, stdCppCustomTypes :: [FilePath]
stdfiles = map (combine stdsrc) ["sys/types.adl", "sys/adlast.adl", "sys/dynamic.adl"]
stdHsCustomTypes = ["../../compiler/lib/adl/sys/types/hs-custom-types.json"]
stdCppCustomTypes = ["../../compiler/lib/adl/sys/types/cpp-custom-types.json"]

runTests :: IO ()
runTests = do
  resultvar <- atomically $ newTVar []
  let collectResults = collectResults1 resultvar

  hspec $ afterAll_ (printRsyncCommands resultvar) $ do
  describe "adlc verify backend" $ do
    it "aborts with error for duplicate definitions of a name" $ do
      runVerifyBackend1 "test8/input/test.adl"
        `shouldReturn` (CompilerFailed "multiple definitions for X")
    it "aborts with error for inconsistent versioned/unversioned definitions of a name" $ do
      runVerifyBackend1 "test9/input/test.adl"
        `shouldReturn` (CompilerFailed "inconsistent version/unversioned definitions for X")
    it "succeeds for correctly numbered versions of a name" $ do
      runVerifyBackend1 "test10/input/test.adl"
        `shouldReturn` MatchOutput
    it "aborts with error for inconsistently numbered versions of a name" $ do
      runVerifyBackend1 "test11/input/test.adl"
        `shouldReturn` (CompilerFailed "inconsistent version numbers for X")
    it "aborts with error for duplicate field names in structs and unions" $ do
      runVerifyBackend1 "test12/input/test.adl"
        `shouldReturn` (CompilerFailed "In module Test :\nduplicate definition of field v in struct X\n  duplicate definition of field v in union Y")
    it "aborts with error for duplicate type parameter names in all decl types" $ do
      runVerifyBackend1 "test13/input/test.adl"
        `shouldReturn` (CompilerFailed "In module Test :\nduplicate definition of type parameter a in struct X\n  duplicate definition of type parameter b in union Y\n  duplicate definition of type parameter t in type alias A\n  duplicate definition of type parameter t in newtype B")
    it "aborts with error for type constructors applied to incorrect numbers of arguments" $ do
      runVerifyBackend1 "test19/input/test.adl"
        `shouldReturn` (CompilerFailed "In module test :\ntype X doesn't take arguments\n  type constructor Pair expected 2 arguments, but was passed 1")

  describe "adlc haskell backend" $ do
    it "generates expected code for an empty module" $ do
      collectResults (runHaskellBackend1 "test1/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for various structures" $ do
      collectResults (runHaskellBackend1 "test2/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runHaskellBackend1 "test3/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      collectResults (runHaskellBackend ["test4/input",stdsrc] ["test4/input/test.adl"] "test4/hs-output")
          `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runHaskellBackend1 "test5/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      let srcs = stdfiles <> ["test6/input/test.adl"]
      collectResults (runHaskellBackend [stdsrc] srcs "test6/hs-output")
          `shouldReturn` MatchOutput
    it "generates expected code for type aliases and newtypes" $ do
      collectResults (runHaskellBackend1 "test7/input/test.adl")
        `shouldReturn` MatchOutput
    it "Generates code correctly for mutually recursive types" $ do
      collectResults (runHaskellBackend1 "test18/input/test.adl")
        `shouldReturn` MatchOutput
    it "Correctly uses specified serialisation field names" $ do
      collectResults (runHaskellBackend1 "test20/input/test.adl")
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runHaskellBackend1 "demo1/input/picture.adl")
        `shouldReturn` MatchOutput

  describe "adlc ast backend" $ do
    it "generates expected json serialisation for each type of decl" $ do
      collectResults (runAstBackend1 "test15/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected json serialisation for custom annotation types" $ do
      collectResults (runAstBackend1 "test21/input/test.adl")
        `shouldReturn` MatchOutput

  describe "adlc cpp backend" $ do
    it "generates expected code for an empty module" $ do
      collectResults (runCppBackend1 "test1/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for various structures" $ do
      collectResults (runCppBackend1 "test2/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runCppBackend1 "test3/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      collectResults (runCppBackend ["test4/input",stdsrc] ["test4/input/test.adl"] "test4/cpp-output" "")
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runCppBackend1 "test5/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      collectResults (runCppBackend [stdsrc] stdfiles "test6/cpp-output" "")
        `shouldReturn` MatchOutput
    it "generates expected code type aliases and newtypes" $ do
      collectResults (runCppBackend1 "test7/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates valid names when ADL contains C++ reserved words" $ do
      collectResults (runCppBackend1 "test14/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates/references include files with a custom prefix" $ do
      collectResults (runCppBackend ["test16/input",stdsrc] ["test16/input/test.adl"] "test16/cpp-output" "adl")
        `shouldReturn` MatchOutput
    it "Expands typedefs in code generation when necessary" $ do
      collectResults (runCppBackend1 "test17/input/test.adl")
        `shouldReturn` MatchOutput
    it "Generates code correctly for mutually recursive types" $ do
      collectResults (runCppBackend1 "test18/input/test.adl")
        `shouldReturn` MatchOutput
    it "Correctly uses specified serialisation field names" $ do
      collectResults (runCppBackend1 "test20/input/test.adl")
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runCppBackend1 "demo1/input/picture.adl")
        `shouldReturn` MatchOutput

  describe "adlc java backend" $ do
    it "generates expected code for various structures" $ do
      collectResults (runJavaBackend1 "test2/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runJavaBackend1 "test3/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      collectResults (runJavaBackend
          ["test4/input",stdsrc] ["test4/input/test.adl"]
          "test4/java-output"
          (withJavaOutputPackage "org.adl")
          )
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runJavaBackend1 "test5/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for type aliases and newtypes" $ do
      collectResults (runJavaBackend1 "test7/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the core standard library" $ do
      let srcs = ["test6/input/test.adl"] <> stdfiles
      collectResults (runJavaBackend [stdsrc] srcs "test6/java-output"
                      (withJavaOutputPackage "org.adl"))
        `shouldReturn` MatchOutput
    it "generates valid names when ADL contains java reserved words" $ do
      collectResults (runJavaBackend1 "test14/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates/references include files with a custom prefix" $ do
      collectResults (runJavaBackend ["test16/input",stdsrc] ["test16/input/test.adl","test16/input/test2.adl"] "test16/java-output" id)
        `shouldReturn` MatchOutput
    it "Expands typedefs in code generation" $ do
      collectResults (runJavaBackend1 "test17/input/test.adl")
        `shouldReturn` MatchOutput
    it "Correctly uses specified serialisation field names" $ do
      collectResults (runJavaBackend1 "test20/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates to packages controlled by annotations" $ do
      collectResults (runJavaBackend
          ["test22/input",stdsrc] ["test22/input/test22a.adl","test22/input/test22b.adl"]
          "test22/java-output"
          (withJavaOutputPackage "org.adl")
          )
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runJavaBackend1 "demo1/input/picture.adl")
        `shouldReturn` MatchOutput

  describe "adlc javascript backend" $ do
    it "generates expected code for the standard library" $ do
      collectResults (runJsBackend [stdsrc] stdfiles "test6/js-output")
        `shouldReturn` MatchOutput
    it "generates expected output for custom annotation types" $ do
      collectResults (runJsBackend [stdsrc] ["test21/input/test.adl"] "test21/js-output")
        `shouldReturn` MatchOutput

  describe "adlc typescript backend" $ do
    it "generates expected output for various structures" $
      collectResults (runTsBackend [stdsrc] ["test2/input/test.adl"] "test2/ts-output")
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runTsBackend [stdsrc] ["test3/input/test.adl"] "test3/ts-output")
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runTsBackend [stdsrc] ["test5/input/test.adl"] "test5/ts-output")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      let srcs = stdfiles <> ["test6/input/test.adl"]
      collectResults (runTsBackend [stdsrc] srcs "test6/ts-output")
          `shouldReturn` MatchOutput
    it "generates expected code for type aliases and newtypes" $ do
      collectResults (runTsBackend [stdsrc] ["test7/input/test.adl"] "test7/ts-output")
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runTsBackend [stdsrc] ["demo1/input/picture.adl"] "demo1/ts-output")
        `shouldReturn` MatchOutput
    it "Handles annotations and docstrings correctly" $ do
      collectResults (runTsBackend [stdsrc] ["test23/input/test23.adl"] "test23/ts-output")
        `shouldReturn` MatchOutput

  describe "adlc rust backend" $ do
    it "Generates the correct code for the picture demo" $ do
      collectResults (runRsBackend [stdsrc] ["demo1/input/picture.adl"] "demo1/rs-output")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      let srcs = stdfiles <> ["test6/input/test.adl"]
      collectResults (runRsBackend [stdsrc] srcs "test6/rs-output")
          `shouldReturn` MatchOutput
    it "generates expected code for type aliases and newtypes" $ do
      collectResults (runRsBackend [stdsrc] ["test7/input/test.adl"] "test7/rs-output")
        `shouldReturn` MatchOutput
 
  where
    collectResults1 resultvar test = do
      r <- test
      atomically $ do
        modifyTVar' resultvar (r:)
      return r

    printRsyncCommands resultvar  = do
      results <- atomically $ readTVar resultvar
      putStrLn "\n** Rsync commands to update"
      mapM_ printRsyncCommand results

    printRsyncCommand (OutputDiff expected actual _) = putStrLn ("rsync -r --delete " <> actual <> "/ " <> expected <> "/")
    printRsyncCommand _ = return ()

main :: IO ()
main = do
  setCurrentDirectory "tests"
  runTests
