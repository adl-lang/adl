{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified ADL.Compiler.Backends.Verify as V
import qualified ADL.Compiler.Backends.Haskell as H
import qualified ADL.Compiler.Backends.AST as A
import qualified ADL.Compiler.Backends.Cpp as C
import qualified ADL.Compiler.Backends.Java as J
import qualified ADL.Compiler.Backends.Javascript as JS
import qualified ADL.Compiler.Backends.Typescript as TS
import qualified ADL.Compiler.Backends.Rust as RS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Paths_adl_compiler as P
import qualified ADL.Compiler.ParserP as PP

import ADL.Compiler.DataFiles
import ADL.Compiler.EIO
import ADL.Compiler.Flags
import ADL.Compiler.AST
import ADL.Compiler.Processing(AdlFlags(..),defaultAdlFlags, parseModuleName)
import ADL.Compiler.Utils
import Control.Monad.Trans
import Data.List(intercalate,partition)
import Data.Monoid
import Data.String(IsString(..))
import Data.Version(showVersion)
import HaskellCustomTypes
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.FilePath(joinPath)
import System.IO(stderr)

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
    "The language package where the ADL runtime is located"

includeRuntimePackageOption ufn =
  Option "" ["include-rt"]
    (NoArg ufn)
    "Generate the runtime code"

astCombinedOutputFile ufn =
  Option "" ["combined-output"]
    (ReqArg ufn "OUTFILE")
    "The json file to which all adl modules will be written"

runVerify args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
      libDir <- liftIO $ getLibDir
      let af = stdAdlFlags libDir []
      let flags = buildFlags af () opts
      moduleNames <- mapM parseModuleName args
      V.verify (f_adl flags) moduleNames
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adlc verify [OPTION...] modules..."

    optDescs =
      [ searchDirOption addToSearchPath
      , mergeFileExtensionOption addToMergeFileExtensions
      ]

runAst args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir []
  (flags,moduleNames) <- parseArguments2 header af (flags0 libDir) (mkOptDescs libDir) args
  withManifest (f_output flags) $ \ writer -> do
    A.generate (f_adl flags) (f_backend flags) writer moduleNames
  where
    header = "Usage: adlc ast [OPTION...] modules..."

    flags0 libDir = A.AstFlags {
      A.astf_combinedModuleFile = Nothing
    }

    mkOptDescs libDir =
      standardOptions <> [
        astCombinedOutputFile (\f -> updateBackendFlags (\astf -> astf{A.astf_combinedModuleFile=Just f}))
      ]


runHaskell args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-hs"]
  (flags,moduleNames) <- parseArguments2 header af (flags0 libDir) (mkOptDescs libDir) args
  withManifest (f_output flags) $ \ writer -> do
    H.generate (f_adl flags) (f_backend flags) writer getCustomType moduleNames
  where
    header = "Usage: adlc haskell [OPTION...] modules..."

    flags0 libDir = H.HaskellFlags
      { H.hf_modulePrefix="ADL.Generated"
      , H.hf_includeRuntime=Nothing
      , H.hf_runtimePackage="ADL.Core"
      }

    mkOptDescs libDir =
      standardOptions <>
      [ generateTransitiveOption setGenerateTransitive
      , outputPackageOption (\s -> updateBackendFlags (\hf -> hf{H.hf_modulePrefix=s}))
      , includeRuntimePackageOption (updateBackendFlags (\hf ->hf{H.hf_includeRuntime=Just (haskellRuntimeDir libDir)}))
      , runtimePackageOption (\s -> updateBackendFlags (\hf -> hf{H.hf_runtimePackage=T.pack s}))
      ]

runCpp args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-cpp"]
  (flags,moduleNames) <- parseArguments2 header af (flags0 libDir) optDescs args
  withManifest (f_output flags) $ \ writer -> do
    C.generate (f_adl flags) (f_backend flags) writer moduleNames
  where
    header = "Usage: adlc cpp [OPTION...] modules..."

    flags0 libDir = C.CppFlags {
      C.cf_incFilePrefix="",
      C.cf_includeRelops=True
      }

    optDescs =
      standardOptions <>
      [ generateTransitiveOption setGenerateTransitive
      , includePrefixOption (\s -> updateBackendFlags (\cf -> cf{C.cf_incFilePrefix=s}))
      , excludeRelopsOption (updateBackendFlags (\cf -> cf{C.cf_includeRelops=False}))
      ]

    includePrefixOption ufn =
      Option "" ["include-prefix"]
        (ReqArg ufn "DIR")
        "The prefix to be used to generate/reference include files"

    excludeRelopsOption ufn =
      Option "" ["exclude-relops"]
        (NoArg ufn)
        "Exclude generated code for relational operators"

runJava args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-java"]
  (flags,paths) <- parseArguments header af (flags0 libDir) optDescs args
  withManifest (f_output flags) $ \ writer -> do
    J.generate (f_adl flags) (f_backend flags) writer paths
  where
    header = "Usage: adlc java [OPTION...] files..."

    flags0 libDir = J.JavaFlags {
      J.jf_libDir=libDir,
      J.jf_package = "adl",
      J.jf_includeRuntime = False,
      J.jf_codeGenProfile = J.defaultCodeGenProfile
    }

    optDescs =
      standardOptions <>
      [ generateTransitiveOption setGenerateTransitive
      , outputPackageOption (\s -> updateBackendFlags (\jf -> jf{J.jf_package=J.javaPackage (T.pack s)}))
      , includeRuntimePackageOption (updateBackendFlags (\jf ->jf{J.jf_includeRuntime=True}))
      , runtimePackageOption (\s -> updateCodeGenProfile (\cgp -> cgp{J.cgp_runtimePackage=fromString s}))
      , javaGenerateParcelable (updateCodeGenProfile (\cgp->cgp{J.cgp_parcelable=True}))
      , javaHungarianNaming (updateCodeGenProfile (\cgp->cgp{J.cgp_hungarianNaming=True}))
      , javaMaxLineLength (\s -> (updateCodeGenProfile (\cgp -> cgp{J.cgp_maxLineLength=read s})))
      , javaHeaderComment (\s -> (updateCodeGenProfile (\cgp -> cgp{J.cgp_header=T.pack s})))
      , javaSuppressWarningsAnnotation (\s -> (updateCodeGenProfile (\cgp -> cgp{J.cgp_supressWarnings=T.splitOn "," (T.pack s)})))
      ]

    javaGenerateParcelable ufn =
      Option "" ["parcelable"]
        (NoArg ufn)
        "Generated java code will include android parcellable implementations"

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

    updateCodeGenProfile f = updateBackendFlags (\jf ->jf{J.jf_codeGenProfile=f (J.jf_codeGenProfile jf)})

runJavascript args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-js"]
  (flags,moduleNames) <- parseArguments2 header af (flags0 libDir) optDescs args
  withManifest (f_output flags) $ \writer -> do
    JS.generate (f_adl flags) (f_backend flags) writer (moduleNames)

  where
    header = "Usage: adlc javascript [OPTION...] modules..."

    flags0 libDir = JS.JavascriptFlags {
    }

    optDescs = standardOptions

runTypescript args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-ts"]
  (flags,moduleNames) <- parseArguments2 header af (flags0 libDir) optDescs args
  withManifest (f_output flags) $ \ writer -> do
    TS.generate (f_adl flags) (f_backend flags) writer moduleNames
  where
    header = "Usage: adlc typescript [OPTION...] modules..."

    flags0 libDir = TS.TypescriptFlags {
      TS.tsStyle=TS.Tsc,
      TS.tsLibDir=libDir,
      TS.tsIncludeRuntime=False,
      TS.tsIncludeResolver=False,
      TS.tsExcludeAst=False,
      TS.tsExcludedAstAnnotations=Nothing,
      TS.tsRuntimeDir=""
    }

    optDescs =
      standardOptions <>
      [ generateTransitiveOption setGenerateTransitive
      , includeRuntimeOption (updateBackendFlags (\tsf ->tsf{TS.tsIncludeRuntime=True}))
      , tsStyle (\s -> updateBackendFlags (\tsf -> tsf{TS.tsStyle=parseStyle s}))
      , tsIncludeResolverOption (updateBackendFlags (\tsf ->tsf{TS.tsIncludeResolver=True}))
      , tsExcludeAstOption (updateBackendFlags (\tsf ->tsf{TS.tsExcludeAst=True}))
      , tsExcludedAstAnnotationsOption (\s -> updateBackendFlags (\tsf ->tsf{TS.tsExcludedAstAnnotations=Just (parseScopedNames s)}))
      , tsRuntimeDirectoryOption (\path -> updateBackendFlags (\tsf ->tsf{TS.tsRuntimeDir=path}))
      ]

    tsIncludeResolverOption ufn =
      Option "" ["include-resolver"]
        (NoArg ufn)
        "Generate the resolver map for all generated adl files"

    tsExcludeAstOption ufn =
      Option "" ["exclude-ast"]
        (NoArg ufn)
        "Exclude the generated ASTs"

    tsExcludedAstAnnotationsOption ufn =
      Option "" ["excluded-ast-annotations"]
        (ReqArg ufn "SCOPEDNAMES")
        "Set the annotations to be excluded from the ast (comma separated, default=sys.annotations.Doc)"

    tsRuntimeDirectoryOption ufn =
      Option "R" ["runtime-dir"]
        (ReqArg ufn "DIR")
        "Set the directory where runtime code is written"


    tsStyle ufn =
      Option "" ["ts-style"]
        (ReqArg ufn "tsc|deno")
        "Select the style of typescript to be generated"

    parseStyle :: String -> TS.TypescriptStyle
    parseStyle s = case s of
      "deno" -> TS.Deno
      "tsc" -> TS.Tsc
      "template" -> TS.Template
      _ -> error "invalid ts-style"

    parseScopedNames :: String -> [ScopedName]
    parseScopedNames s = case PP.fromString (PP.parseScopedNameList) s of
      Left e -> error "Unable to parse scoped names"
      Right sns -> sns

runRust args = do
  libDir <- liftIO $ getLibDir
  let af = stdAdlFlags libDir ["adl-rs"]
  (flags,moduleNames) <- parseArguments2 header af (flags0 libDir) optDescs args
  withManifest (f_output flags) $ \ writer -> do
    RS.generate (f_adl flags) (f_backend flags) writer moduleNames
  where
    header = "Usage: adlc rust [OPTION...] modules..."

    flags0 libDir = RS.RustFlags {
      RS.rs_libDir = libDir,
      RS.rs_module = RS.rustScopedName "adl",
      RS.rs_runtimeModule = RS.rustScopedName "adlrt",
      RS.rs_includeRuntime = False
    }

    optDescs =
      standardOptions <>
      [ generateTransitiveOption setGenerateTransitive
      , rsModuleOption (\s -> updateBackendFlags (\rf -> rf{RS.rs_module=RS.rustScopedName (T.pack s)}))
      , rsRuntimeModuleOption (\s -> updateBackendFlags (\rf -> rf{RS.rs_runtimeModule=RS.rustScopedName (T.pack s)}))
      , includeRuntimeOption (updateBackendFlags (\rf ->rf{RS.rs_includeRuntime=True}))
      ]

    rsModuleOption ufn =
      Option "R" ["module"]
        (ReqArg ufn "DIR")
        "Set the module where the code is generated, relative to crate root"

    rsRuntimeModuleOption ufn =
      Option "R" ["runtime-module"]
        (ReqArg ufn "DIR")
        "Set the module where the runtime is located, relative to crate root"

runShow args0 =
  case args0 of
    ["--adlstdlib"] -> liftIO $ do
      systemAdlDir <- systemAdlDir <$> getLibDir
      putStrLn systemAdlDir
    ["--version"] -> liftIO $ do
      putStrLn (showVersion P.version)
    _ -> eioError "Usage: adlc show [OPTION...]"

usage = T.intercalate "\n"
  [ "Usage: adlc verify [OPTION..] <module>..."
  , "       adlc ast [OPTION..] <module>..."
  , "       adlc haskell [OPTION..] <module>..."
  , "       adlc cpp [OPTION..] <module>..."
  , "       adlc java [OPTION..] <modulePath>..."
  , "       adlc javascript [OPTION..] <module>..."
  , "       adlc typescript [OPTION..] <module>..."
  , "       adlc rust [OPTION..] <module>..."
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
    ("rust":args) -> runRust args
    ("show":args) -> runShow args
    _ -> eioError usage
  where
    runEIO eio = do
      a <- unEIO $ eio
      case a of
        (Left perr) ->
          T.hPutStrLn stderr perr >> exitWith (ExitFailure 1)
        (Right _) -> exitWith ExitSuccess
