{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.GetOpt
import System.Exit
import System.Environment (getArgs)
import System.FilePath(joinPath)
import Data.List(intercalate,partition)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import EIO
import Compiler

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
    (opts,args,[]) -> verify (mkFlags opts) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl verify [OPTION...] files..."
    
    mkFlags opts = (foldl (.) id opts) (VerifyFlags [])

    optDescs =
      [ searchDirOption (\s vf-> vf{vf_searchPath=s:vf_searchPath vf})
      ]

runHaskell args0 =
  case getOpt Permute optDescs args0 of
    (opts,args,[]) -> haskell (mkFlags opts) args
    (_,_,errs) -> eioError (T.pack (concat errs ++ usageInfo header optDescs))
  where
    header = "Usage: adl haskell [OPTION...] files..."
    
    mkFlags opts = (foldl (.) id opts) (HaskellFlags [] "ADL.Generated" "." False)

    optDescs =
      [ searchDirOption (\s hf-> hf{hf_searchPath=s:hf_searchPath hf})
      , Option "" ["moduleprefix"]
        (ReqArg (\s hf-> hf{hf_modulePrefix=s}) "PREFIX")
        "Set module name prefix for generated code "
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


      