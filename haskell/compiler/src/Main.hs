{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import Control.Monad.Trans
import System.Exit
import System.Environment (getArgs)
import System.FilePath(joinPath)
import Data.List(intercalate,partition)
import Data.String(IsString(..))

import qualified Data.Text as T
import qualified Data.Text.IO as T

import ADL.Compiler.EIO
import ADL.Compiler.Backends.Verify as V
import ADL.Compiler.Backends.Haskell as H
import ADL.Compiler.Backends.AST as A
import ADL.Compiler.Backends.Cpp as C
import ADL.Compiler.Backends.Java as J
import ADL.Compiler.DataFiles
import ADL.Compiler.Utils
import HaskellCustomTypes

searchDirOption ufn =
  Option "I" ["searchdir"]
    (ReqArg ufn "DIR")
    "Add the specifed directory to the ADL searchpath"

includePrefixOption ufn =
  Option "" ["include-prefix"]
    (ReqArg ufn "DIR")
    "The prefix to be used to generate/reference include files"

outputDirOption ufn =
  Option "O" ["outputdir"]
    (ReqArg ufn "DIR")
    "Set the directory where generated code is written"

customTypesOption ufn =
  Option "" ["custom-types"]
    (ReqArg ufn "FILE")
    "Read custom type mapping from the specified file"

noOverwriteOption ufn =
  Option "" ["no-overwrite"]
    (NoArg ufn)
    "Don't update files that haven't changed"

javaPackageOption ufn =
  Option "" ["package"]
    (ReqArg ufn "PACKAGE")
    "The java package into which the generated ADL code will be placed"

javaRuntimePackageOption ufn =
  Option "" ["rtpackage"]
    (ReqArg ufn "PACKAGE")
    "The java package where the ADL runtime is located"

javaIncludeRuntimePackageOption ufn =
  Option "" ["include-rt"]
    (NoArg ufn)
    "Generate the runtime code"

javaGenerateParcelable ufn =
  Option "" ["parcelable"]
    (NoArg ufn)
    "Generated java code will include android parcellable implementations"

javaGenerateJson ufn =
  Option "" ["json"]
    (NoArg ufn)
    "Generated java code will include gson json serialization"

javaHeaderComment ufn =
  Option "" ["header-comment"]
    (ReqArg ufn "PACKAGE")
    "A comment to be placed at the start of each java file"

javaHungarianNaming ufn =
  Option "" ["hungarian-naming"]
    (NoArg ufn)
    "Use hungarian naming conventions"

javaMaxLineLength ufn =
  Option "" ["max-line-length"]
    (ReqArg ufn "PACKAGE")
    "The maximum length of the generated code lines"

runVerify args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> V.verify (mkFlags opts) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl verify [OPTION...] files..."
    
    mkFlags opts = (foldl (.) id opts) (V.VerifyFlags [])

    optDescs =
      [ searchDirOption (\s vf-> vf{vf_searchPath=s:vf_searchPath vf})
      ]

runAst args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> A.generate (mkFlags opts) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl ast [OPTION...] files..."
    
    mkFlags opts = (foldl (.) id opts) (A.Flags [] (writeOutputFile out0))
                                        
    out0 = OutputArgs {
      oa_log = putStrLn,
      oa_noOverwrite = True,
      oa_outputPath = "."
    }

    optDescs =
      [ searchDirOption (\s f-> f{af_searchPath=s:af_searchPath f})
      ]

runHaskell args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
        flags <- mkFlags opts
        H.generate flags getCustomTypes args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl haskell [OPTION...] files..."

    mkFlags opts = do
      stdCustomTypes <- liftIO $ getDataFileName "config/hs-custom-types.json"
      let (flags1,out1) = (foldl (.) id opts) (flags0 [stdCustomTypes],out0)
      return flags1{hf_fileWriter=writeOutputFile out1}

    flags0 ctypes = H.HaskellFlags {
      hf_searchPath=[],
      hf_modulePrefix="ADL.Generated",
      hf_customTypeFiles=ctypes,
      hf_fileWriter= \_ _ -> return ()
    }
    out0 = OutputArgs {
      oa_log = putStrLn,
      oa_noOverwrite = True,
      oa_outputPath = "."
    }

    optDescs =
      [ searchDirOption (\s (hf,o)-> (hf{hf_searchPath=s:hf_searchPath hf},o))
      , Option "" ["moduleprefix"]
        (ReqArg (\s (hf,o)-> (hf{hf_modulePrefix=s},o)) "PREFIX")
        "Set module name prefix for generated code "
      , customTypesOption (\s (hf,o)-> (hf{hf_customTypeFiles=s:hf_customTypeFiles hf},o))
      , outputDirOption (\s (hf,o)-> (hf,o{oa_outputPath=s}))
      , noOverwriteOption (\(hf,o)-> (hf,o{oa_noOverwrite=True}))
      ]

runCpp args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
        flags <- mkFlags opts
        C.generate flags args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl cpp [OPTION...] files..."

    mkFlags opts = do
      stdCustomTypes <- liftIO $ getDataFileName "config/cpp-custom-types.json"
      let (flags1,out) = (foldl (.) id opts) (flags0 [stdCustomTypes],out0)
      return flags1{cf_fileWriter=writeOutputFile out}

    flags0 ctypes = C.CppFlags {
      cf_searchPath=[],
      cf_incFilePrefix="",
      cf_customTypeFiles=ctypes,
      cf_fileWriter= \_ _ -> return ()
    }
    out0 = OutputArgs {
      oa_log = putStrLn,
      oa_noOverwrite = True,
      oa_outputPath = "."
    }

    optDescs =
      [ searchDirOption (\s (cf,o)-> (cf{cf_searchPath=s:cf_searchPath cf},o))
      , customTypesOption (\s (cf,o)-> (cf{cf_customTypeFiles=s:cf_customTypeFiles cf},o))
      , includePrefixOption (\s (cf,o)-> (cf{cf_incFilePrefix=s},o))
      , outputDirOption (\s (cf,o)-> (cf,o{oa_outputPath=s}))
      , noOverwriteOption (\(cf,o)-> (cf,o{oa_noOverwrite=True}))
      ]

runJava args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
        flags <- mkFlags opts
        J.generate flags args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl java [OPTION...] files..."

    mkFlags opts = do
      let (flags1,out) = (foldl (.) id opts) (flags0,out0)
      return flags1{jf_fileWriter=writeOutputFile out}

    flags0 = J.JavaFlags {
      jf_searchPath=[],
      jf_customTypeFiles=[],
      jf_package = "adl",
      jf_fileWriter= \_ _ -> return (),
      jf_includeRuntime = False,
      jf_codeGenProfile = J.defaultCodeGenProfile
    }
    out0 = OutputArgs {
      oa_log = putStrLn,
      oa_noOverwrite = True,
      oa_outputPath = "."
    }

    optDescs =
      [ searchDirOption (\s (jf,o)-> (jf{jf_searchPath=s:jf_searchPath jf},o))
      , outputDirOption (\s (jf,o)-> (jf,o{oa_outputPath=s}))
      , customTypesOption (\s (jf,o)-> (jf{jf_customTypeFiles=s:jf_customTypeFiles jf},o))
      , noOverwriteOption (\(jf,o)-> (jf,o{oa_noOverwrite=True}))
      , javaPackageOption (\s (jf,o) -> (jf{jf_package=T.pack s},o))
      , javaRuntimePackageOption (\s (jf,o) ->(jf{jf_codeGenProfile=(jf_codeGenProfile jf){cgp_runtimePackage=fromString s}},o))
      , javaIncludeRuntimePackageOption (\(jf,o) ->(jf{jf_includeRuntime=True},o))
      , javaGenerateParcelable (\(jf,o) ->(jf{jf_codeGenProfile=(jf_codeGenProfile jf){cgp_parcelable=True}},o))
      , javaGenerateJson (\(jf,o) ->(jf{jf_codeGenProfile=(jf_codeGenProfile jf){cgp_json=True}},o))
      , javaHungarianNaming (\(jf,o) ->(jf{jf_codeGenProfile=(jf_codeGenProfile jf){cgp_hungarianNaming=True}},o))
      , javaMaxLineLength (\s (jf,o) ->(jf{jf_codeGenProfile=(jf_codeGenProfile jf){cgp_maxLineLength=read s}},o))
      , javaHeaderComment (\s (jf,o) ->(jf{jf_codeGenProfile=(jf_codeGenProfile jf){cgp_header=T.pack s}},o))
      ]

usage = T.intercalate "\n"
  [ "Usage: adl verify [OPTION..] <modulePath>..."
  , "       adl ast [OPTION..] <modulePath>..."
  , "       adl haskell [OPTION..] <modulePath>..."
  , "       adl cpp [OPTION..] <modulePath>..."
  , "       adl java [OPTION..] <modulePath>..."
  ]    
    
main = do
  args <- getArgs
  runEIO $ case args of
    ("verify":args) -> runVerify args
    ("haskell":args) -> runHaskell args
    ("ast":args) -> runAst args
    ("cpp":args) -> runCpp args
    ("java":args) -> runJava args
    _ -> eioError usage
  where
    runEIO eio = do
      a <- unEIO $ eio
      case a of
        (Left perr) ->
          T.putStrLn perr >> exitWith (ExitFailure 1)
        (Right _) -> exitWith ExitSuccess

      
