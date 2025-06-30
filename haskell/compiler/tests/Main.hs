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
import ADL.Compiler.Processing(AdlFlags(..),defaultAdlFlags,parseModuleName)

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

runVerifyBackend :: [FilePath] -> [String] -> IO CodeGenResult
runVerifyBackend ipath moduleNameStrs = do
  let af =  defaultAdlFlags{af_searchPath=[stdsrc] <> ipath}
  er <- unEIO $ do
    moduleNames <- mapM parseModuleName moduleNameStrs
    V.verify af moduleNames
  case er of
   (Left err) -> return (CompilerFailed err)
   (Right ()) -> return MatchOutput

runHaskellBackend :: [FilePath] -> [String] -> FilePath -> IO CodeGenResult
runHaskellBackend ipaths moduleNameStrs epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af =  defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=["adl-hs"]}
      hf =  H.HaskellFlags
        { H.hf_modulePrefix = "ADL"
        , H.hf_includeRuntime = Nothing
        , H.hf_runtimePackage = "ADL.Core"
        }
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir Nothing)
  er <- unEIO $ do
    moduleNames <- mapM parseModuleName moduleNameStrs
    H.generate af hf fileWriter getCustomType moduleNames
  processCompilerOutput epath tempDir er

runCppBackend :: [FilePath] -> [FilePath] -> FilePath -> FilePath -> IO CodeGenResult
runCppBackend ipaths mpaths epath iprefix = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af =  defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=["adl-cpp"]}
      cf = CPP.CppFlags {
        CPP.cf_incFilePrefix = iprefix,
        CPP.cf_includeRelops = True
        }
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir Nothing)
  er <- unEIO $ CPP.generate af cf fileWriter mpaths
  processCompilerOutput epath tempDir er

runCppBackend1 :: FilePath-> IO CodeGenResult
runCppBackend1 mpath = runCppBackend [ipath,stdsrc] [mpath] epath ""
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "cpp-output"

runAstBackend :: [FilePath] -> [String] -> FilePath -> IO CodeGenResult
runAstBackend ipath moduleNameStrs epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af = defaultAdlFlags{af_searchPath=[stdsrc] <> ipath,af_mergeFileExtensions=[".ann"]}
      bf = AST.AstFlags Nothing
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir Nothing)
  er <- unEIO $ do
    moduleNames <- mapM parseModuleName moduleNameStrs
    AST.generate af bf fileWriter moduleNames
  processCompilerOutput epath tempDir er

runJavaBackend :: [FilePath] -> [FilePath] -> FilePath -> (J.JavaFlags -> J.JavaFlags) -> IO CodeGenResult
runJavaBackend ipaths mpaths epath updateflags = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let af = defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=["adl-java"]}
      jf = J.JavaFlags {
        J.jf_libDir = "LIBDIR",
        J.jf_package = J.javaPackage "adl",
        J.jf_includeRuntime = False,
        J.jf_codeGenProfile = J.defaultCodeGenProfile
        }
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir Nothing)
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
      fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir Nothing)
  er <- unEIO $ JS.generate af jf fileWriter mpaths
  processCompilerOutput epath tempDir er

runTsBackend :: (TS.TypescriptFlags -> TS.TypescriptFlags) -> [FilePath] -> [FilePath] -> FilePath -> IO CodeGenResult
runTsBackend flagsfn ipaths mpaths epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adlt.test."
  let af = defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=[]}
      js = flagsfn $ TS.TypescriptFlags {
        TS.tsStyle=TS.Tsc,
        TS.tsIncludeRuntime=False,
        TS.tsIncludeResolver=True,
        TS.tsRuntimeDir="",
        TS.tsLibDir="../../../haskell/compiler/lib",
        TS.tsExcludeAst=False,
        TS.tsExcludedAstAnnotations=Nothing
      }
      fileWriter = writeOutputFile (OutputArgs (\_ -> return ()) False tempDir Nothing)
  er <- unEIO $ TS.generate af js fileWriter mpaths
  processCompilerOutput epath tempDir er

runRsBackend :: [FilePath] -> [String] -> FilePath -> T.Text -> IO CodeGenResult
runRsBackend ipaths moduleNameStrs epath rsModule = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adlt.test."
  let af = defaultAdlFlags{af_searchPath=ipaths,af_mergeFileExtensions=["adl-rs"]}
      js = RS.RustFlags {
        RS.rs_libDir = ".",
        RS.rs_module = RS.rustScopedName rsModule,
        RS.rs_runtimeModule = RS.rustScopedName "adlrt",
        RS.rs_includeRuntime = False
      }
      fileWriter = writeOutputFile (OutputArgs (\_ -> return ()) False tempDir Nothing)
  er <- unEIO $ do
    moduleNames <- mapM parseModuleName moduleNameStrs
    RS.generate af js fileWriter moduleNames
  processCompilerOutput epath tempDir er

stdsrc :: FilePath
stdsrc = "../../../haskell/compiler/lib/adl"

stdfiles, stdHsCustomTypes, stdCppCustomTypes :: [FilePath]
stdfiles = map (combine stdsrc) ["sys/types.adl", "sys/adlast.adl", "sys/dynamic.adl"]
stdHsCustomTypes = ["../../compiler/lib/adl/sys/types/hs-custom-types.json"]
stdCppCustomTypes = ["../../compiler/lib/adl/sys/types/cpp-custom-types.json"]

stdModules :: [String]
stdModules = ["sys.types", "sys.adlast", "sys.dynamic"]

runTests :: IO ()
runTests = do
  resultvar <- atomically $ newTVar []
  let collectResults = collectResults1 resultvar

  hspec $ afterAll_ (printRsyncCommands resultvar) $ do
  describe "adlc verify backend" $ do
    it "aborts with error for duplicate definitions of a name" $ do
      runVerifyBackend ["test8/input"] ["test"]
        `shouldReturn` (CompilerFailed "multiple definitions for X")
    it "aborts with error for inconsistent versioned/unversioned definitions of a name" $ do
      runVerifyBackend ["test9/input"] ["test"]
        `shouldReturn` (CompilerFailed "inconsistent version/unversioned definitions for X")
    it "succeeds for correctly numbered versions of a name" $ do
      runVerifyBackend ["test10/input"] ["test"]
        `shouldReturn` MatchOutput
    it "aborts with error for inconsistently numbered versions of a name" $ do
      runVerifyBackend ["test11/input"] ["test"]
        `shouldReturn` (CompilerFailed "inconsistent version numbers for X")
    it "aborts with error for duplicate field names in structs and unions" $ do
      runVerifyBackend ["test12/input"] ["test"]
        `shouldReturn` (CompilerFailed "In module Test :\nduplicate definition of field v in struct X\n  duplicate definition of field v in union Y")
    it "aborts with error for duplicate type parameter names in all decl types" $ do
      runVerifyBackend ["test13/input"] ["test"]
        `shouldReturn` (CompilerFailed "In module Test :\nduplicate definition of type parameter a in struct X\n  duplicate definition of type parameter b in union Y\n  duplicate definition of type parameter t in type alias A\n  duplicate definition of type parameter t in newtype B")
    it "aborts with error for type constructors applied to incorrect numbers of arguments" $ do
      runVerifyBackend ["test19/input"] ["test"]
        `shouldReturn` (CompilerFailed "In module test :\ntype X doesn't take arguments\n  type constructor Pair expected 2 arguments, but was passed 1")
    it "aborts with error with extra content at end of module file" $ do
      runVerifyBackend ["test28/input"] ["test28"]
        `shouldReturn` (CompilerFailed "\"test28/input/test28.adl\" (line 4, column 1):\nunexpected 's'\nexpecting space, \"//\" or end of input")

  describe "adlc haskell backend" $ do
    it "generates expected code for an empty module" $ do
      collectResults (runHaskellBackend [stdsrc, "test1/input"] ["test1"] "test1/hs-output")
        `shouldReturn` MatchOutput
    it "generates expected code for various structures" $ do
      collectResults (runHaskellBackend [stdsrc, "test2/input"] ["test2"] "test2/hs-output" )
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runHaskellBackend [stdsrc, "test3/input"] ["test3"] "test3/hs-output" )
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      collectResults (runHaskellBackend [stdsrc, "test4/input"] ["test4/input/test4"] "test4/hs-output")
          `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runHaskellBackend [stdsrc, "test5/input"] ["test5"] "test5/hs-output" )
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      collectResults (runHaskellBackend [stdsrc, "test6/input"] (stdModules <> ["test6"]) "test6/hs-output")
          `shouldReturn` MatchOutput
    it "generates expected code for type aliases and newtypes" $ do
      collectResults (runHaskellBackend [stdsrc, "test7/input"] ["test7"] "test7/hs-output" )
        `shouldReturn` MatchOutput
    it "Generates code correctly for mutually recursive types" $ do
      collectResults (runHaskellBackend [stdsrc, "test18/input"] ["test18"] "test18/hs-output" )
        `shouldReturn` MatchOutput
    it "Correctly uses specified serialisation field names" $ do
      collectResults (runHaskellBackend [stdsrc, "test20/input"] ["test20"] "test20/hs-output" )
        `shouldReturn` MatchOutput
    it "generated code for type token primitives" $ do
      collectResults (runHaskellBackend [stdsrc, "test24/input"] ["test24"] "test24/hs-output" )
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runHaskellBackend [stdsrc, "demo1/input"] ["picture"] "demo1/hs-output" )
        `shouldReturn` MatchOutput
    it "generates correct keys for stringmap literals" $ do
      collectResults (runHaskellBackend [stdsrc, "test29/input"] ["test29"] "test29/hs-output" )
        `shouldReturn` MatchOutput

  describe "adlc ast backend" $ do
    it "generates expected json serialisation for each type of decl" $ do
      collectResults (runAstBackend ["test15/input"] ["test"] "test15/ast-output")
        `shouldReturn` MatchOutput
    it "generates expected json serialisation for custom annotation types" $ do
      collectResults (runAstBackend ["test21/input"] ["test"] "test21/ast-output")
        `shouldReturn` MatchOutput

  describe "adlc cpp backend" $ do
    it "generates expected code for an empty module" $ do
      collectResults (runCppBackend1 "test1/input/test1.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for various structures" $ do
      collectResults (runCppBackend1 "test2/input/test2.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runCppBackend1 "test3/input/test3.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      collectResults (runCppBackend ["test4/input",stdsrc] ["test4/input/test4.adl"] "test4/cpp-output" "")
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runCppBackend1 "test5/input/test5.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for nullable" $ do
      collectResults (runCppBackend1 "test6/input/test6.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      collectResults (runCppBackend [stdsrc] stdfiles "test6/cpp-output-std" "")
        `shouldReturn` MatchOutput
    it "generates expected code type aliases and newtypes" $ do
      collectResults (runCppBackend1 "test7/input/test7.adl")
        `shouldReturn` MatchOutput
    it "generates valid names when ADL contains C++ reserved words" $ do
      collectResults (runCppBackend1 "test14/input/test14.adl")
        `shouldReturn` MatchOutput
    it "generates/references include files with a custom prefix" $ do
      collectResults (runCppBackend ["test16/input",stdsrc] ["test16/input/test.adl"] "test16/cpp-output" "adl")
        `shouldReturn` MatchOutput
    it "Expands typedefs in code generation when necessary" $ do
      collectResults (runCppBackend1 "test17/input/test17.adl")
        `shouldReturn` MatchOutput
    it "Generates code correctly for mutually recursive types" $ do
      collectResults (runCppBackend1 "test18/input/test18.adl")
        `shouldReturn` MatchOutput
    it "Correctly uses specified serialisation field names" $ do
      collectResults (runCppBackend1 "test20/input/test20.adl")
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runCppBackend1 "demo1/input/picture.adl")
        `shouldReturn` MatchOutput
    it "generates correct keys for stringmap literals" $ do
      collectResults (runCppBackend1 "test29/input/test29.adl")
        `shouldReturn` MatchOutput

  describe "adlc java backend" $ do
    it "generates expected code for various structures" $ do
      collectResults (runJavaBackend1 "test2/input/test2.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runJavaBackend1 "test3/input/test3.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      collectResults (runJavaBackend
          ["test4/input",stdsrc] ["test4/input/test4.adl"]
          "test4/java-output"
          (withJavaOutputPackage "org.adl")
          )
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runJavaBackend1 "test5/input/test5.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for type aliases and newtypes" $ do
      collectResults (runJavaBackend1 "test7/input/test7.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the core standard library" $ do
      let srcs = ["test6/input/test6.adl"] <> stdfiles
      collectResults (runJavaBackend [stdsrc] srcs "test6/java-output"
                      (withJavaOutputPackage "org.adl"))
        `shouldReturn` MatchOutput
    it "generates valid names when ADL contains java reserved words" $ do
      collectResults (runJavaBackend1 "test14/input/test14.adl")
        `shouldReturn` MatchOutput
    it "generates/references include files with a custom prefix" $ do
      collectResults (runJavaBackend ["test16/input",stdsrc] ["test16/input/test.adl","test16/input/test2.adl"] "test16/java-output" id)
        `shouldReturn` MatchOutput
    it "Expands typedefs in code generation" $ do
      collectResults (runJavaBackend1 "test17/input/test17.adl")
        `shouldReturn` MatchOutput
    it "Correctly uses specified serialisation field names" $ do
      collectResults (runJavaBackend1 "test20/input/test20.adl")
        `shouldReturn` MatchOutput
    it "generates to packages controlled by annotations" $ do
      collectResults (runJavaBackend
          ["test22/input",stdsrc] ["test22/input/test22a.adl","test22/input/test22b.adl"]
          "test22/java-output"
          (withJavaOutputPackage "org.adl")
          )
        `shouldReturn` MatchOutput
    it "generated code for type token primitives" $ do
      collectResults (runJavaBackend1 "test24/input/test24.adl")
        `shouldReturn` MatchOutput
    it "generated code for type aliases accross modules" $ do
      collectResults (runJavaBackend ["test25/input",stdsrc] ["test25/input/admin.adl"] "test25/java-output" id)
        `shouldReturn` MatchOutput
    it "generated code for SerializedWithInternalTag union annotation" $ do
      collectResults (runJavaBackend1 "test26/input/test26.adl")
        `shouldReturn` MatchOutput
    it "generated code for types that reference another type of the same name" $ do
      collectResults (runJavaBackend1 "test27/input/test27.adl")
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runJavaBackend1 "demo1/input/picture.adl")
        `shouldReturn` MatchOutput
    it "generates correct keys for stringmap literals" $ do
      collectResults (runJavaBackend1 "test29/input/test29.adl")
        `shouldReturn` MatchOutput
    it "doesn't intefer with template parameters in literal strings" $ do
      collectResults (runJavaBackend1 "test30/input/test30.adl")
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
      collectResults (runTsBackend id [stdsrc] ["test2/input/test2.adl"] "test2/ts-output")
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runTsBackend id [stdsrc] ["test3/input/test3.adl"] "test3/ts-output")
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runTsBackend id [stdsrc] ["test5/input/test5.adl"] "test5/ts-output")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      let srcs = stdfiles <> ["test6/input/test6.adl"]
      collectResults (runTsBackend id [stdsrc] srcs "test6/ts-output")
          `shouldReturn` MatchOutput
    it "generates expected code for type aliases and newtypes" $ do
      collectResults (runTsBackend id [stdsrc] ["test7/input/test7.adl"] "test7/ts-output")
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runTsBackend id [stdsrc] ["demo1/input/picture.adl"] "demo1/ts-output")
        `shouldReturn` MatchOutput
    it "Handles annotations and docstrings correctly" $ do
      collectResults (runTsBackend id [stdsrc] ["test23/input/test23.adl"] "test23/ts-output")
        `shouldReturn` MatchOutput
    it "generates code for type token primitives" $ do
      collectResults (runTsBackend id [stdsrc] ["test24/input/test24.adl"] "test24/ts-output")
        `shouldReturn` MatchOutput
    it "generates correct keys for stringmap literals" $ do
      collectResults (runTsBackend (\tf -> tf{TS.tsExcludedAstAnnotations=Just []}) [stdsrc] ["test29/input/test29.adl"] "test29/ts-output")
        `shouldReturn` MatchOutput

  describe "adlc rust backend" $ do
    it "generates expected code for an empty module" $ do
      collectResults (runRsBackend [stdsrc, "test1/input"] ["test1"] "test1/rs-output" "test1::adl")
        `shouldReturn` MatchOutput
    it "generates expected output for various structures" $
      collectResults (runRsBackend [stdsrc, "test2/input"] ["test2"] "test2/rs-output" "test2::adl")
        `shouldReturn` MatchOutput
    it "generates expected code for structures with default overrides" $ do
      collectResults (runRsBackend [stdsrc, "test3/input"] ["test3"] "test3/rs-output" "test3::adl")
        `shouldReturn` MatchOutput
    it "generates expected code for custom type mappings" $ do
      collectResults (runRsBackend [stdsrc, "test4/input"] ["test4", "sys.types"] "test4/rs-output" "test4::adl")
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runRsBackend [stdsrc, "test5/input"] ["test5"] "test5/rs-output" "test5::adl")
        `shouldReturn` MatchOutput
    it "generates expected code for type aliases and newtypes" $ do
      collectResults (runRsBackend [stdsrc, "test7/input"] ["test7"] "test7/rs-output" "test7::adl")
        `shouldReturn` MatchOutput
    it "Generates code correctly for mutually recursive types" $ do
      collectResults (runRsBackend [stdsrc, "test18/input"] ["test18"] "test18/rs-output" "test18::adl")
        `shouldReturn` MatchOutput
    it "generates valid names when ADL contains rust reserved words" $ do
      collectResults (runRsBackend [stdsrc, "test14/input"] ["test14"] "test14/rs-output" "test14::adl")
        `shouldReturn` MatchOutput
    it "Correctly uses specified serialisation field names" $ do
      collectResults (runRsBackend [stdsrc,"test20/input"] ["test20"] "test20/rs-output" "test20::adl")
        `shouldReturn` MatchOutput
    it "Generates the correct code for the picture demo" $ do
      collectResults (runRsBackend [stdsrc,"demo1/input"] ["picture"] "demo1/rs-output" "demo1::adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      let modules = stdModules <> ["test6"]
      collectResults (runRsBackend [stdsrc, "test6/input"] modules "test6/rs-output" "test6::adl")
          `shouldReturn` MatchOutput
    it "generates correct keys for stringmap literals" $ do
      collectResults (runRsBackend [stdsrc,"test29/input"] ["test29"] "test29/rs-output" "test29::adl")
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
