{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import Control.Monad.Trans
import System.Exit
import System.Environment (getArgs)
import System.FilePath(joinPath)
import Data.List(intercalate,partition)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import ADL.Compiler.EIO
import ADL.Compiler.Utils
import ADL.Compiler.Backends.Verify as V
import ADL.Compiler.Backends.Haskell as H
import ADL.Compiler.Backends.Cpp as C
import HaskellCustomTypes
import Paths_adl_compiler

searchDirOption ufn =
  Option "I" ["searchdir"]
    (ReqArg ufn "DIR")
    "Add the specifed directory to the ADL searchpath"

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
      , outputDirOption (\s (cf,o)-> (cf,o{oa_outputPath=s}))
      , noOverwriteOption (\(cf,o)-> (cf,o{oa_noOverwrite=True}))
      ]

usage = T.intercalate "\n"
  [ "Usage: adl verify [OPTION..] <modulePath>..."
  , "       adl haskell [OPTION..] <modulePath>..."
  , "       adl cpp [OPTION..] <modulePath>..."
  ]    
    
main = do
  args <- getArgs
  runEIO $ case args of
    ("verify":args) -> runVerify args
    ("haskell":args) -> runHaskell args
    ("cpp":args) -> runCpp args
    _ -> eioError usage
  where
    runEIO eio = do
      a <- unEIO $ eio
      case a of
        (Left perr) ->
          T.putStrLn perr >> exitWith (ExitFailure 1)
        (Right _) -> exitWith ExitSuccess


      