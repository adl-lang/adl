{-# LANGUAGE OverloadedStrings #-}
module HaskellCustomTypes where

import Control.Monad.Trans

import qualified Data.Map as Map
import qualified Data.Text as T

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Utils.Format

import ADL.Compiler.Backends.Haskell

import qualified Text.Parsec as P
import qualified ADL.Compiler.ParserP as P

import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Set as Set

import ADL.Core.Value
import ADL.Compiler.Processing
import qualified ADL.Adlc.Config.Haskell as HC

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType scopedName decl = case Map.lookup haskellCustomType (d_annotations decl) of
  Nothing -> Nothing
  Just (_,json) -> Just (convertCustomType json)
  where
    haskellCustomType = ScopedName (ModuleName ["adlc","config","haskell"]) "HaskellCustomType"

    convertCustomType :: JSON.Value -> CustomType
    convertCustomType jv = case aFromJSON (jsonSerialiser (JSONFlags True)) jv of
      Nothing -> error "BUG: failed to parse java custom type"
      (Just hct) -> CustomType {
        ct_hTypeName = (HC.haskellCustomType_haskellname hct),
        ct_hImports = map HaskellModule (HC.haskellCustomType_haskellimports hct),
        ct_insertCode = (HC.haskellCustomType_insertCode hct),
        ct_generateOrigADLType = convertOrigType (HC.haskellCustomType_generateOrigADLType hct)
        }
      where
        convertOrigType ot | ot == "" = Nothing
                           | otherwise = Just ot
                    
    scopedName :: T.Text -> ScopedName
    scopedName t = case P.parse P.scopedName "" t of
          (Right sn) -> sn
          _ -> error "failed to parse scoped name in haskell custom type"
