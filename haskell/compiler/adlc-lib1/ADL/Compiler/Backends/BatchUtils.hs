{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.BatchUtils where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as JSON
import System.FilePath

import ADL.Compiler.AST
import ADL.Utils.IndentedCode
import ADL.Compiler.EIO
import ADL.Compiler.DataFiles
import ADL.Compiler.Primitive
import ADL.Compiler.Processing
import ADL.Compiler.Utils
import ADL.Core.Value
import ADL.Core.Nullable(unNullable)
import ADL.Utils.FileDiff(dirContents)
import ADL.Utils.Format
import ADL.Adlc.Codegen.Types(AdlSources, AdlTreeSource(..),OutputParams(..))

batchLogFn  :: Bool -> LogFn
batchLogFn verbose = case verbose of
  True -> putStrLn
  False -> \s -> return ()

batchModuleLoader :: LogFn -> AdlSources -> [T.Text] -> ModuleLoader
batchModuleLoader log sources mergeExts =   mergedModuleLoader
  [ modulesFromDirectory (T.unpack path) (map T.unpack mergeExts)
    | (AdlTreeSource_localDir path) <- sources
  ]

batchModuleFinder :: LogFn -> AdlSources -> [T.Text] -> ModuleFinder
batchModuleFinder log sources mergeExts = ModuleFinder {
  mf_finder = modulePathCandidates [T.unpack path | (AdlTreeSource_localDir path) <- sources],
  mf_log = log,
  mf_mergeFileExtensions = map T.unpack mergeExts
}

withBatchFileWriter :: LogFn -> OutputParams -> (FileWriter -> EIOT a) -> EIOT a
withBatchFileWriter log oparams fn = do
  wwm <- liftIO $ batchFileWriter log oparams
  a <- fn (wwm_writeFile wwm)
  liftIO $ wwm_writeManifest wwm
  return a

batchFileWriter :: LogFn -> OutputParams -> IO WriterWithManifest
batchFileWriter log oparams = writerWithManifest $ OutputArgs {
  oa_log = log,
  oa_noOverwrite = outputParams_noOverwrite oparams,
  oa_outputPath = outputPath,
  oa_manifestFile = fmap (outputPath </>) manifestFile
  }
  where
    manifestFile = fmap T.unpack (unNullable (outputParams_manifest oparams))
    outputPath = T.unpack (outputParams_outputDir oparams)

moduleNameFromText :: T.Text -> ModuleName
moduleNameFromText = ModuleName . T.splitOn "."