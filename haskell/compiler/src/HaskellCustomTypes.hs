{-# LANGUAGE OverloadedStrings #-}
module HaskellCustomTypes where

import Control.Monad.Trans
import Data.Monoid

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
  Just (_,json) -> case validateCustomType (convertCustomType json) of
    Left err -> error (T.unpack err)
    Right ct -> Just ct
  where
    haskellCustomType = ScopedName (ModuleName ["adlc","config","haskell"]) "HaskellCustomType"

    convertCustomType :: JSON.Value -> CustomType
    convertCustomType jv = case adlFromJson jv of
      (ParseFailure e ctx) -> error (T.unpack (  "BUG: failed to parse java custom type: " <> e
                                    <> ", at " <> textFromParseContext ctx))
      (ParseSuccess hct) -> CustomType {
        ct_hTypeName = (HC.haskellCustomType_haskellname hct),
        ct_hImports = map HaskellModule (HC.haskellCustomType_haskellimports hct),
        ct_hExtraExports = (HC.haskellCustomType_haskellextraexports hct),
        ct_insertCode = (HC.haskellCustomType_insertCode hct),
        ct_structConstructor = HC.haskellCustomType_structConstructor hct,
        ct_unionConstructors = Map.fromList
          [ (HC.unionConstructor_fieldName uc, HC.unionConstructor_constructor uc)
          | uc <- HC.haskellCustomType_unionConstructors hct],
        ct_generateOrigADLType = convertOrigType (HC.haskellCustomType_generateOrigADLType hct)
        }
      where
        convertOrigType ot | ot == "" = Nothing
                           | otherwise = Just ot

    -- check whether the custom types for unions/structs have the constructors
    -- that match the decl
    validateCustomType :: CustomType -> Either T.Text CustomType
    validateCustomType ct = case d_type decl of
      Decl_Struct _ -> checkNonNullConstructor
      Decl_Newtype _ -> checkNonNullConstructor
      Decl_Union u
        | Set.null (missingCtors u) -> Right ct
        | otherwise -> Left (
            "Union custom type for " <> d_name decl
            <> " missing constructors: " <> T.intercalate ", " (Set.toList (missingCtors u)))
      _ -> Right ct
      where
        checkNonNullConstructor
          | T.null (ct_structConstructor ct) =
              Left ("Custom type for " <> d_name decl <> " is missing constructor")
          | otherwise = Right ct
        requiredCtors u = Set.fromList (map f_name (u_fields u))
        customCtors = Set.fromList (Map.keys (ct_unionConstructors ct))
        missingCtors u = Set.difference (requiredCtors u) customCtors
                       
    scopedName :: T.Text -> ScopedName
    scopedName t = case P.parse P.scopedName "" t of
          (Right sn) -> sn
          _ -> error "failed to parse scoped name in haskell custom type"
