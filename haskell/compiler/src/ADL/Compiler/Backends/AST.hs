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
import qualified ADL.Core.StringMap as SM
import Data.Text.Lazy.Builder

import Control.Monad
import Control.Monad.Trans

import ADL.Compiler.EIO
import ADL.Compiler.AST
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Utils
import ADL.Compiler.ExternalAST(moduleNameToA2, moduleToA2)

import ADL.Core.Value

import qualified ADL.Sys.Adlast as A2

data AstFlags = AstFlags {
  astf_combinedModuleFile :: Maybe FilePath
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

generate :: AdlFlags -> AstFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af astFlags fileWriter modulePaths = case astf_combinedModuleFile astFlags of
  Nothing -> generateIndividualModuleFiles af fileWriter modulePaths
  (Just file) -> generateCombinedModuleFile af (fileWriter file) modulePaths

generateIndividualModuleFiles af fileWriter modulePaths = do
  catchAllExceptions  $ forM_ modulePaths $ \modulePath -> do
    rm <- loadAndCheckModule af modulePath
    writeModuleFile fileWriter rm

-- Write a combined single json file containing the specified modules, and all of the
-- ADL files upon which they depend. The json in the file will have type
--    StringMap<AST.Module>
generateCombinedModuleFile af writeFile modulePaths = do
  allModules <- catchAllExceptions  $ forM modulePaths (loadAndCheckModule1 af)
  let modulesByName = mconcat (map merge1 allModules)
  liftIO $ writeFile (JSON.encodePretty' JSON.defConfig (adlToJson modulesByName))
  where
    keyedModule m = SM.singleton (moduleNameToA2 (m_name m)) (moduleToA2 (fullyScopedModule m))
    merge1 (m,ms) = mconcat (keyedModule m : (map keyedModule ms))
