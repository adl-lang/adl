{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.AST(
  AstFlags(..),
  generate,
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as S
import Data.Text.Lazy.Builder

import Control.Monad
import Control.Monad.Trans

import ADL.Compiler.EIO
import ADL.Compiler.AST
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Utils
import ADL.Compiler.ExternalAST(moduleToA2)

import ADL.Core.Value

import qualified ADL.Sys.Adlast as A2

data AstFlags = AstFlags {
  astf_fileWriter :: FilePath -> LBS.ByteString -> IO ()
  }

writeModuleFile :: (FilePath -> LBS.ByteString -> IO ()) ->
                   RModule ->
                   EIO a ()
writeModuleFile fileWriter m = do
  let adlast = moduleToA2 m
      v = adlToJson adlast
      fpath =  T.unpack (T.intercalate "." (unModuleName (m_name m) )) ++ ".json"

      -- JSON output in sorted keyname order, as we use the
      -- output in the unit tests
      encodeDef = JSON.defConfig{JSON.confCompare=compare}

  liftIO $ fileWriter fpath (JSON.encodePretty' encodeDef v)

generate :: AdlFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af fileWriter modulePaths = catchAllExceptions  $ forM_ modulePaths $ \modulePath -> do
  rm <- loadAndCheckModule af modulePath
  writeModuleFile fileWriter rm
