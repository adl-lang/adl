{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import System.Exit
import System.Environment (getArgs)
import System.FilePath(joinPath)
import System.IO(stderr)
import Data.List(intercalate,partition)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import ADL.Compiler.EIO
import ADL.Compiler.Utils
import ADL.Compiler.Processing(AdlFlags(..),defaultAdlFlags)
import ADL.Compiler.Backends.Haskell as H
import ADL.Compiler.Backends.Verify as V
import BootstrapCustomTypes

searchDirOption ufn =
  Option "I" ["searchdir"]
    (ReqArg ufn "DIR")
    "Add the specifed directory to the ADL searchpath"

outputDirOption ufn =
  Option "O" ["outputdir"]
    (ReqArg ufn "DIR")
    "Set the directory where generated code is written"

noOverwriteOption ufn =
  Option "" ["no-overwrite"]
    (NoArg ufn)
    "Don't update files that haven't changed"

runVerify args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> V.verify (mkFlags opts) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl verify [OPTION...] files..."

    mkFlags opts = (foldl (.) id opts) defaultAdlFlags

    optDescs =
      [ searchDirOption (\s af-> af{af_searchPath=s:af_searchPath af})
      ]

runHaskell args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> do
      let (af,flags,out) = mkFlags opts
      H.generate af flags (writeOutputFile out) getCustomType args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl haskell [OPTION...] files..."

    mkFlags opts = (foldl (.) id opts) (defaultAdlFlags,flags0,out0)

    flags0 = H.HaskellFlags {
      hf_modulePrefix="ADL.Generated",
      hf_includeRuntime=Nothing,
      hf_runtimePackage="ADL.Core"
    }
    out0 = OutputArgs {
      oa_log = putStrLn,
      oa_noOverwrite = True,
      oa_outputPath = ".",
      oa_manifestFile = Nothing
    }

    optDescs =
      [ searchDirOption (\s (af,hf,o)-> (af{af_searchPath=s:af_searchPath af},hf,o))
      , Option "" ["package"]
        (ReqArg (\s (af,hf,o)-> (af,hf{hf_modulePrefix=s},o)) "PREFIX")
        "Set module name prefix for generated code "
      , outputDirOption (\s (af,hf,o)-> (af,hf,o{oa_outputPath=s}))
      , noOverwriteOption (\(af,hf,o)-> (af,hf,o{oa_noOverwrite=True}))
      ]

usage = T.intercalate "\n"
  [ "Usage: adl verify [OPTION..] <modulePath>..."
  , "       adl haskell [OPTION..] <modulePath>..."
  ]

main = do
  args <- getArgs
  runEIO $ case args of
    ("verify":args) -> runVerify args
    ("haskell":args) -> runHaskell args
    _ -> eioError usage
  where
    runEIO eio = do
      a <- unEIO $ eio
      case a of
        (Left perr) ->
          T.hPutStrLn stderr perr >> exitWith (ExitFailure 1)
        (Right _) -> exitWith ExitSuccess
