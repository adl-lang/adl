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

import qualified ADL.Compiler.Backends.Verify as V
import qualified ADL.Compiler.Backends.Haskell as H
import qualified ADL.Compiler.Backends.Cpp as CPP
import qualified ADL.Compiler.Backends.AST as AST
import qualified ADL.Compiler.Backends.Java as J

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
  let flags =  V.VerifyFlags {
    V.vf_searchPath = ipath
    }
  er <- unEIO $ V.verify flags mpaths
  case er of
   (Left err) -> return (CompilerFailed err)
   (Right ()) -> return MatchOutput

runVerifyBackend1 :: FilePath -> IO CodeGenResult
runVerifyBackend1 mpath = runVerifyBackend [stdsrc,takeDirectory mpath] [mpath]

runHaskellBackend :: [FilePath] -> [FilePath] -> FilePath -> [FilePath] -> IO CodeGenResult
runHaskellBackend ipaths mpaths epath customTypeFiles = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let flags =  H.HaskellFlags {
    H.hf_searchPath = ipaths,
    H.hf_modulePrefix = "ADL",
    H.hf_customTypeFiles = customTypeFiles,
    H.hf_fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
    }
  er <- unEIO $ H.generate flags getCustomTypes mpaths
  processCompilerOutput epath tempDir er

runHaskellBackend1 :: FilePath-> IO CodeGenResult
runHaskellBackend1 mpath = runHaskellBackend [ipath,stdsrc] [mpath] epath []
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "hs-output"

runCppBackend :: [FilePath] -> [FilePath] -> FilePath -> FilePath -> [FilePath] -> IO CodeGenResult
runCppBackend ipaths mpaths epath iprefix customTypeFiles = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let flags = CPP.CppFlags {
    CPP.cf_searchPath = ipaths,
    CPP.cf_customTypeFiles = customTypeFiles,
    CPP.cf_incFilePrefix = iprefix,
    CPP.cf_fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
    }
  er <- unEIO $ CPP.generate flags mpaths
  processCompilerOutput epath tempDir er

runCppBackend1 :: FilePath-> IO CodeGenResult
runCppBackend1 mpath = runCppBackend [ipath,stdsrc] [mpath] epath "" []
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "cpp-output"

runAstBackend :: [FilePath] -> [FilePath] -> FilePath -> IO CodeGenResult
runAstBackend ipath mpaths epath = do
  tdir <- getTemporaryDirectory
  tempDir <- createTempDirectory tdir "adl.test."
  let flags = AST.Flags {
    AST.af_searchPath = ipath,
    AST.af_fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir)
    }
  er <- unEIO $ AST.generate flags mpaths
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
  let flags = J.JavaFlags {
    J.jf_searchPath = ipaths,
    J.jf_libDir = "LIBDIR",
    J.jf_customTypeFiles = [],
    J.jf_package = J.javaPackage "adl",
    J.jf_fileWriter = writeOutputFile (OutputArgs (\_-> return ()) False tempDir),
    J.jf_includeRuntime = False,
    J.jf_codeGenProfile = J.defaultCodeGenProfile {J.cgp_json=True}
    }
  er <- unEIO $ J.generate (updateflags flags) mpaths
  processCompilerOutput epath tempDir er 

withJavaCustomTypeFiles :: [FilePath] -> J.JavaFlags -> J.JavaFlags
withJavaCustomTypeFiles files flags = flags{J.jf_customTypeFiles=files}

withJavaOutputPackage :: T.Text -> J.JavaFlags -> J.JavaFlags
withJavaOutputPackage package flags = flags{J.jf_package=J.javaPackage package}

runJavaBackend1 :: FilePath -> IO CodeGenResult
runJavaBackend1 mpath = runJavaBackend [ipath,stdsrc] [mpath] epath id
  where
    ipath = takeDirectory mpath
    epath = (takeDirectory ipath) </> "java-output"

stdsrc :: FilePath
stdsrc = "../../../adl/stdlib"

stdfiles, stdHsCustomTypes, stdCppCustomTypes :: [FilePath]
stdfiles = map (combine stdsrc) ["sys/types.adl", "sys/rpc.adl", "sys/sinkimpl.adl", "sys/adlast.adl"]
stdHsCustomTypes = ["../../compiler/lib/adl/sys/types/hs-custom-types.json"]
stdCppCustomTypes = ["../../compiler/lib/adl/sys/types/cpp-custom-types.json"]
stdJavaCustomTypes = ["../../compiler/lib/adl/sys/types/java-custom-types.json"]

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
      collectResults (runHaskellBackend ["test4/input",stdsrc] ["test4/input/test.adl"] "test4/hs-output" (stdHsCustomTypes++["test4/input/hs-custom-types.json"]))
          `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runHaskellBackend1 "test5/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      collectResults (runHaskellBackend [stdsrc] stdfiles "test6/hs-output" stdHsCustomTypes)
          `shouldReturn` MatchOutput
    it "generates expected code type aliases and newtypes" $ do
      collectResults (runHaskellBackend1 "test7/input/test.adl")
        `shouldReturn` MatchOutput
    it "Generates code correctly for mutually recursive types" $ do
      collectResults (runHaskellBackend1 "test18/input/test.adl")
        `shouldReturn` MatchOutput
    it "Correctly uses specified serialisation field names" $ do
      collectResults (runHaskellBackend1 "test20/input/test.adl")
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
      collectResults (runCppBackend ["test4/input",stdsrc] ["test4/input/test.adl"] "test4/cpp-output" "" (stdCppCustomTypes ++ ["test4/input/cpp-custom-types.json"]))
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runCppBackend1 "test5/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the standard library" $ do
      collectResults (runCppBackend [stdsrc] stdfiles "test6/cpp-output" "" stdCppCustomTypes)
        `shouldReturn` MatchOutput
    it "generates expected code type aliases and newtypes" $ do
      collectResults (runCppBackend1 "test7/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates valid names when ADL contains C++ reserved words" $ do
      collectResults (runCppBackend1 "test14/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates/references include files with a custom prefix" $ do
      collectResults (runCppBackend ["test16/input",stdsrc] ["test16/input/test.adl"] "test16/cpp-output" "adl" [])
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
          (withJavaCustomTypeFiles (["test4/input/java-custom-types.json"]++stdJavaCustomTypes)
          .withJavaOutputPackage "org.adl")
          )
        `shouldReturn` MatchOutput
    it "generates expected code for various unions" $ do
      collectResults (runJavaBackend1 "test5/input/test.adl")
        `shouldReturn` MatchOutput
    it "generates expected code for the core standard library" $ do
      collectResults (runJavaBackend [stdsrc] (map (combine stdsrc) ["sys/types.adl","sys/adlast.adl"]) "test6/java-output"
                      (withJavaCustomTypeFiles stdJavaCustomTypes
                      .withJavaOutputPackage "org.adl"))
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
    it "Generates the correct code for the picture demo" $ do
      collectResults (runJavaBackend1 "demo1/input/picture.adl")
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
