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
import ADL.Compiler.Processing(AdlFlags(..),defaultAdlFlags)
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

getDefaultAdlFlags :: EIO T.Text (AdlFlags)
getDefaultAdlFlags = do
  systemAdlDir <- liftIO (systemAdlDir <$> getLibDir)
  return defaultAdlFlags
     { af_searchPath=[systemAdlDir]
     , af_log=putStrLn
     }

defaultOutputArgs :: OutputArgs
defaultOutputArgs = OutputArgs {
  oa_log = putStrLn,
  oa_noOverwrite = True,
  oa_outputPath = "."
  }

-- Hold a set of flags for the adl frontend, a given backend
-- and the output writer
data Flags b = Flags {
  f_adl :: AdlFlags,
  f_output :: OutputArgs,
  f_backend :: b
  }

updateAdlFlags :: (AdlFlags -> AdlFlags) -> Flags b -> Flags b
updateAdlFlags fn flags = flags{f_adl=fn (f_adl flags)}

updateOutputArgs :: (OutputArgs -> OutputArgs) -> Flags b -> Flags b
updateOutputArgs fn flags = flags{f_output=fn (f_output flags)}

updateBackendFlags :: (b -> b) -> Flags b -> Flags b
updateBackendFlags fn flags = flags{f_backend=fn (f_backend flags)}

addToSearchPath :: FilePath -> Flags b -> Flags b
addToSearchPath path = updateAdlFlags (\af-> af{af_searchPath=path:af_searchPath af})

setOutputDir :: FilePath -> Flags b -> Flags b
setOutputDir dir = updateOutputArgs (\oa -> oa{oa_outputPath=dir})

setNoOverwrite :: Flags b -> Flags b
setNoOverwrite = updateOutputArgs (\oa-> oa{oa_noOverwrite=True})

-- | Combine an initial set of AdlFlags and appropriate backend
-- flags with command line arguments.
buildFlags :: AdlFlags -> b -> [Flags b -> Flags b] -> Flags b
buildFlags af0 b0 opts = ((foldl (.) id opts) (Flags af0 defaultOutputArgs b0))

runVerify args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
      af <- getDefaultAdlFlags
      let flags = buildFlags af () opts
      V.verify (f_adl flags) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl verify [OPTION...] files..."
    
    optDescs =
      [ searchDirOption addToSearchPath
      ]

runAst args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
      af <- getDefaultAdlFlags
      let flags = buildFlags af () opts
      A.generate (f_adl flags) (writeOutputFile (f_output flags)) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl ast [OPTION...] files..."
    
    optDescs =
      [ searchDirOption addToSearchPath
      ]

runHaskell args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
        libDir <- liftIO $ getLibDir
        af <- getDefaultAdlFlags
        let flags = buildFlags af (flags0 libDir) opts
        H.generate (f_adl flags) (f_backend flags) (writeOutputFile (f_output flags)) getCustomTypes args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl haskell [OPTION...] files..."

    flags0 libDir = H.HaskellFlags {
      hf_modulePrefix="ADL.Generated"
      }

    optDescs =
      [ searchDirOption addToSearchPath
      , outputDirOption setOutputDir
      , noOverwriteOption setNoOverwrite
      , Option "" ["moduleprefix"]
        (ReqArg (\s -> updateBackendFlags (\hf -> hf{hf_modulePrefix=s})) "PREFIX")
        "Set module name prefix for generated code "
      ]

runCpp args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
        libDir <- liftIO $ getLibDir
        af <- getDefaultAdlFlags
        let flags = buildFlags af (flags0 libDir) opts
        C.generate (f_adl flags) (f_backend flags) (writeOutputFile (f_output flags)) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl cpp [OPTION...] files..."

    flags0 libDir = C.CppFlags {
      cf_incFilePrefix="",
      cf_customTypeFiles= [stdlibCustomTypesCpp libDir],
      cf_fileWriter= \_ _ -> return ()
      }

    optDescs =
      [ searchDirOption addToSearchPath
      , outputDirOption setOutputDir
      , noOverwriteOption setNoOverwrite
      , customTypesOption (\s -> updateBackendFlags (\cf -> cf{cf_customTypeFiles=s:cf_customTypeFiles cf}))
      , includePrefixOption (\s -> updateBackendFlags (\cf -> cf{cf_incFilePrefix=s}))
      ]

runJava args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
        libDir <- liftIO $ getLibDir
        af <- getDefaultAdlFlags
        let flags = buildFlags af (flags0 libDir) opts
        J.generate (f_adl flags) (f_backend flags) (writeOutputFile (f_output flags)) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl java [OPTION...] files..."

    flags0 libDir = J.JavaFlags {
      jf_libDir=libDir,
      jf_package = "adl",
      jf_includeRuntime = False,
      jf_codeGenProfile = J.defaultCodeGenProfile
    }

    optDescs =
      [ searchDirOption addToSearchPath
      , outputDirOption setOutputDir
      , noOverwriteOption setNoOverwrite
      , javaPackageOption (\s -> updateBackendFlags (\jf -> jf{jf_package=javaPackage (T.pack s)}))
      , javaIncludeRuntimePackageOption (updateBackendFlags (\jf ->jf{jf_includeRuntime=True}))
      , javaRuntimePackageOption (\s -> updateCodeGenProfile (\cgp -> cgp{cgp_runtimePackage=fromString s}))
      , javaGenerateParcelable (updateCodeGenProfile (\cgp->cgp{cgp_parcelable=True}))
      , javaGenerateJson (updateCodeGenProfile (\cgp->cgp{cgp_json=True}))
      , javaHungarianNaming (updateCodeGenProfile (\cgp->cgp{cgp_hungarianNaming=True}))
      , javaMaxLineLength (\s -> (updateCodeGenProfile (\cgp -> cgp{cgp_maxLineLength=read s})))
      , javaHeaderComment (\s -> (updateCodeGenProfile (\cgp -> cgp{cgp_header=T.pack s})))
      ]

    updateCodeGenProfile f = updateBackendFlags (\jf ->jf{jf_codeGenProfile=f (jf_codeGenProfile jf)})

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

      
