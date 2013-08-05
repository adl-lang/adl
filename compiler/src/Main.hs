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
import ADL.Compiler.Backends.Haskell as H
import ADL.Compiler.Backends.Verify as V
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
      stdCustomTypes <- liftIO $ getDataFileName "config/custom_types.json"
      return $ (foldl (.) id opts) (H.HaskellFlags [] "ADL.Generated" [stdCustomTypes] "." False)

    optDescs =
      [ searchDirOption (\s hf-> hf{hf_searchPath=s:hf_searchPath hf})
      , Option "" ["moduleprefix"]
        (ReqArg (\s hf-> hf{hf_modulePrefix=s}) "PREFIX")
        "Set module name prefix for generated code "
      , customTypesOption (\s hf-> hf{hf_customTypeFiles=s:hf_customTypeFiles hf})
      , outputDirOption (\s hf-> hf{hf_outputPath=s})
      , noOverwriteOption (\hf-> hf{hf_noOverwrite=True})
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
          T.putStrLn perr >> exitWith (ExitFailure 1)
        (Right _) -> exitWith ExitSuccess


      