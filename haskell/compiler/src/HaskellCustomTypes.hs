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

getCustomTypes :: RModule -> CustomTypeMap
getCustomTypes rm = foldModule
  where
    foldModule = Map.unions [foldDecl Set.empty (ScopedName (m_name rm) n) d | (n,d) <- Map.toList (m_decls rm)]
      where
        foldDecl inProgress declName decl = Map.union (foldAnnotations (d_annotations decl)) (foldDeclType (d_type decl))
          where
            foldAnnotations anns = case Map.lookup haskellCustomType anns of
              Nothing -> Map.empty
              Just (_,json) -> Map.singleton declName (convertCustomType json)
              
            foldDeclType (Decl_Struct s) = Map.unions (map (foldTypeExpr . f_type) (s_fields s))
            foldDeclType (Decl_Union u) = Map.unions (map (foldTypeExpr . f_type) (u_fields u))
            foldDeclType (Decl_Typedef t) = foldTypeExpr (t_typeExpr t)
            foldDeclType (Decl_Newtype n) = foldTypeExpr (n_typeExpr n)

            foldTypeExpr (TypeExpr t tes) = Map.unions (foldResolvedType t:map foldTypeExpr tes)

            foldResolvedType (RT_Named (sn,decl))
              = if Set.member sn inProgress
                   then Map.empty
                   else foldDecl (Set.insert sn inProgress) sn decl
            foldResolvedType _ = Map.empty

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
