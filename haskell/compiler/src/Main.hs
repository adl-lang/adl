{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import Control.Monad.Trans
import System.Exit
import System.Environment (getArgs)
import System.FilePath(joinPath)
import Data.Monoid
import Data.List(intercalate,partition)
import Data.String(IsString(..))
import Data.Version(showVersion)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import ADL.Compiler.EIO
import ADL.Compiler.Flags
import ADL.Compiler.Backends.Verify as V
import ADL.Compiler.Backends.Haskell as H
import ADL.Compiler.Backends.AST as A
import ADL.Compiler.Backends.Cpp as C
import ADL.Compiler.Backends.Java as J
import ADL.Compiler.Backends.Javascript as JS
import ADL.Compiler.Backends.Typescript as TS
import ADL.Compiler.DataFiles
import ADL.Compiler.Utils
import ADL.Compiler.Processing(AdlFlags(..),defaultAdlFlags)
import HaskellCustomTypes

import qualified Paths_adl_compiler as P

stdAdlFlags :: FilePath -> [String] -> AdlFlags
stdAdlFlags libDir mergeFileExtensions =
  defaultAdlFlags
   { af_searchPath=[systemAdlDir libDir]
   , af_mergeFileExtensions=mergeFileExtensions
   }

outputPackageOption ufn =
  Option "" ["package"]
    (ReqArg ufn "PACKAGE")
    "The language package into which the generated ADL code will be placed"

runtimePackageOption ufn =
  Option "" ["rtpackage"]
    (ReqArg ufn "PACKAGE")
    "The java package where the ADL runtime is located"

includeRuntimePackageOption ufn =
  Option "" ["include-rt"]
    (NoArg ufn)
    "Generate the runtime code"

runVerify args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
      libDir <- liftIO $ getLibDir
      let af = stdAdlFlags libDir []
      let flags = buildFlags af () opts
      V.verify (f_adl flags) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adlc verify [OPTION...] files..."

    optDescs =
      [ searchDirOption addToSearchPath
      , mergeFileExtensionOption addToMergeFileExtensions
      ]

runAst args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
      libDir <- liftIO $ getLibDir
      let af = stdAdlFlags libDir ["adl-hs"]
      let flags = buildFlags af () opts
      A.generate (f_adl flags) (writeOutputFile (f_output flags)) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adlc ast [OPTION...] files..."

    optDescs = standardOptions


runHaskell args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-hs"]
  (flags,paths) <- parseArguments header af (flags0 libDir) (mkOptDescs libDir) args
  H.generate (f_adl flags) (f_backend flags) (writeOutputFile (f_output flags)) getCustomType paths
  where
    header = "Usage: adlc haskell [OPTION...] files..."

    flags0 libDir = H.HaskellFlags
      { hf_modulePrefix="ADL.Generated"
      , hf_includeRuntime=Nothing
      , hf_runtimePackage="ADL.Core"
      }

    mkOptDescs libDir =
      standardOptions <>
      [ outputPackageOption (\s -> updateBackendFlags (\hf -> hf{hf_modulePrefix=s}))
      , includeRuntimePackageOption (updateBackendFlags (\hf ->hf{hf_includeRuntime=Just (haskellRuntimeDir libDir)}))
      , runtimePackageOption (\s -> updateBackendFlags (\hf -> hf{hf_runtimePackage=T.pack s}))
      ]

runCpp args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-cpp"]
  (flags,paths) <- parseArguments header af (flags0 libDir) optDescs args
  C.generate (f_adl flags) (f_backend flags) (writeOutputFile (f_output flags)) paths
  where
    header = "Usage: adlc cpp [OPTION...] files..."

    flags0 libDir = C.CppFlags {
      cf_incFilePrefix="",
      cf_fileWriter= \_ _ -> return ()
      }

    optDescs =
      standardOptions <>
      [ includePrefixOption (\s -> updateBackendFlags (\cf -> cf{cf_incFilePrefix=s}))
      ]

    includePrefixOption ufn =
      Option "" ["include-prefix"]
        (ReqArg ufn "DIR")
        "The prefix to be used to generate/reference include files"

runJava args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-java"]
  (flags,paths) <- parseArguments header af (flags0 libDir) optDescs args
  J.generate (f_adl flags) (f_backend flags) (writeOutputFile (f_output flags)) paths
  where
    header = "Usage: adlc java [OPTION...] files..."

    flags0 libDir = J.JavaFlags {
      jf_libDir=libDir,
      jf_package = "adl",
      jf_includeRuntime = False,
      jf_codeGenProfile = J.defaultCodeGenProfile
    }

    optDescs =
      standardOptions <>
      [ outputPackageOption (\s -> updateBackendFlags (\jf -> jf{jf_package=javaPackage (T.pack s)}))
      , includeRuntimePackageOption (updateBackendFlags (\jf ->jf{jf_includeRuntime=True}))
      , runtimePackageOption (\s -> updateCodeGenProfile (\cgp -> cgp{cgp_runtimePackage=fromString s}))
      , javaGenerateParcelable (updateCodeGenProfile (\cgp->cgp{cgp_parcelable=True}))
      , javaGenerateJson (updateCodeGenProfile (\cgp->cgp{cgp_json=True}))
      , javaHungarianNaming (updateCodeGenProfile (\cgp->cgp{cgp_hungarianNaming=True}))
      , javaMaxLineLength (\s -> (updateCodeGenProfile (\cgp -> cgp{cgp_maxLineLength=read s})))
      , javaHeaderComment (\s -> (updateCodeGenProfile (\cgp -> cgp{cgp_header=T.pack s})))
      , javaSuppressWarningsAnnotation (\s -> (updateCodeGenProfile (\cgp -> cgp{cgp_supressWarnings=T.splitOn "," (T.pack s)})))
      ]

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

    javaSuppressWarningsAnnotation ufn =
      Option "" ["suppress-warnings-annotation"]
        (ReqArg ufn "WARNINGS")
        "The @SuppressWarnings annotation to be generated (comma separated)"

    updateCodeGenProfile f = updateBackendFlags (\jf ->jf{jf_codeGenProfile=f (jf_codeGenProfile jf)})

runJavascript args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-js"]
  (flags,paths) <- parseArguments header af (flags0 libDir) optDescs args
  JS.generate (f_adl flags) (f_backend flags) (writeOutputFile (f_output flags)) paths
  where
    header = "Usage: adlc javascript [OPTION...] files..."

    flags0 libDir = JS.JavascriptFlags {
    }

    optDescs = standardOptions

runTypescript args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-js"]
  (flags,paths) <- parseArguments header af (flags0 libDir) optDescs args
  TS.generate (f_adl flags) (f_backend flags) (writeOutputFile (f_output flags)) paths
  where
    header = "Usage: adlc typescript [OPTION...] files..."

    flags0 libDir = TS.TypescriptFlags {
      tsLibDir=libDir,
      tsIncludeRuntime=False,
      tsExcludeAst=False,
      tsRuntimeDir=""
    }

    optDescs =
      standardOptions <>
      [ tsIncludeRuntimePackageOption (updateBackendFlags (\tsf ->tsf{tsIncludeRuntime=True}))
      , tsExcludeAstOption (updateBackendFlags (\tsf ->tsf{tsExcludeAst=True}))
      , tsRuntimeDirectoryOption (\path -> updateBackendFlags (\tsf ->tsf{tsRuntimeDir=path}))
      ]

    tsIncludeRuntimePackageOption ufn =
      Option "" ["include-rt"]
        (NoArg ufn)
        "Generate the runtime code"

    tsExcludeAstOption ufn =
      Option "" ["exclude-ast"]
        (NoArg ufn)
        "Exclude the generated ASTs"

    tsRuntimeDirectoryOption ufn =
      Option "R" ["runtime-dir"]
        (ReqArg ufn "DIR")
        "Set the directory where runtime code is written"

runShow args0 =
  case args0 of
    ["--adlstdlib"] -> liftIO $ do
      systemAdlDir <- systemAdlDir <$> getLibDir
      putStrLn systemAdlDir
    ["--version"] -> liftIO $ do
      putStrLn (showVersion P.version)
    _ -> eioError "Usage: adlc show [OPTION...]"

usage = T.intercalate "\n"
  [ "Usage: adlc verify [OPTION..] <modulePath>..."
  , "       adlc ast [OPTION..] <modulePath>..."
  , "       adlc haskell [OPTION..] <modulePath>..."
  , "       adlc cpp [OPTION..] <modulePath>..."
  , "       adlc java [OPTION..] <modulePath>..."
  , "       adlc javascript [OPTION..] <modulePath>..."
  , "       adlc typescript [OPTION..] <modulePath>..."
  , "       adlc show --version"
  , "       adlc show --adlstdlib"
  ]

main = do
  args <- getArgs
  runEIO $ case args of
    ("verify":args) -> runVerify args
    ("haskell":args) -> runHaskell args
    ("ast":args) -> runAst args
    ("cpp":args) -> runCpp args
    ("java":args) -> runJava args
    ("javascript":args) -> runJavascript args
    ("typescript":args) -> runTypescript args
    ("show":args) -> runShow args
    _ -> eioError usage
  where
    runEIO eio = do
      a <- unEIO $ eio
      case a of
        (Left perr) ->
          T.putStrLn perr >> exitWith (ExitFailure 1)
        (Right _) -> exitWith ExitSuccess
