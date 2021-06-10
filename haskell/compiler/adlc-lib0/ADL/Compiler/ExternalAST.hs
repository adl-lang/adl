{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.ExternalAST(
  moduleNameToA2,
  moduleToA2,
  declToA2,
  moduleNameFromA2,
  moduleFromA2,
  scopedNameFromA2,
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JSON
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

import ADL.Core.Value

import qualified ADL.Sys.Adlast as A2

-- | Convert the internal AST into an external one, suitable for serialisation.

moduleToA2 :: RModule -> A2.Module
moduleToA2 m = A2.Module name imports decls annotations
  where
    name = moduleNameToA2 (m_name m)
    imports = map importToA2 (m_imports m)
    decls = SM.StringMap (Map.map declToA2 (m_decls m))
    annotations = annotationsToA2 (m_annotations m)

importToA2 :: Import -> A2.Import
importToA2 (Import_Module mname) = A2.Import_moduleName (moduleNameToA2 mname)
importToA2 (Import_ScopedName sn) = A2.Import_scopedName (scopedNameToA2 sn)

moduleNameToA2 :: ModuleName -> A2.ModuleName
moduleNameToA2 mn = T.intercalate "." (unModuleName mn)

scopedNameToA2 :: ScopedName -> A2.ScopedName
scopedNameToA2 sn =A2.ScopedName (moduleNameToA2 (sn_moduleName sn))  (sn_name sn)

declToA2 :: RDecl -> A2.Decl
declToA2 d = A2.Decl (d_name d) (fmap fromIntegral (d_version d)) (declTypeToA2 (d_type d)) (annotationsToA2 (d_annotations d))

declTypeToA2 :: DeclType ResolvedType -> A2.DeclType
declTypeToA2 (Decl_Struct s) = A2.DeclType_struct_ (A2.Struct (s_typeParams s) (map fieldToA2 (s_fields s)))
declTypeToA2 (Decl_Union u) = A2.DeclType_union_ (A2.Union (u_typeParams u) (map fieldToA2 (u_fields u)))
declTypeToA2 (Decl_Typedef t) = A2.DeclType_type_ (A2.TypeDef (t_typeParams t) (typeExprToA2 (t_typeExpr t)))
declTypeToA2 (Decl_Newtype n) = A2.DeclType_newtype_ (A2.NewType (n_typeParams n) (typeExprToA2 (n_typeExpr n)) (n_default n))

fieldToA2 :: Field ResolvedType -> A2.Field
fieldToA2 f = A2.Field (f_name f) (f_serializedName f) (typeExprToA2 (f_type f)) (f_default f) (annotationsToA2 (f_annotations f))

typeExprToA2 :: TypeExpr ResolvedType -> A2.TypeExpr
typeExprToA2 (TypeExpr rt rts) = A2.TypeExpr (typeRefToA2 rt) (map typeExprToA2 rts)

typeRefToA2 :: ResolvedType -> A2.TypeRef
typeRefToA2 (RT_Named (sn,_)) = A2.TypeRef_reference (scopedNameToA2 sn)
typeRefToA2 (RT_Param i) = A2.TypeRef_typeParam i
typeRefToA2 (RT_Primitive p) = A2.TypeRef_primitive (ptToText p)

annotationsToA2 :: Map.Map ScopedName (ResolvedType,JSON.Value) -> Map.Map A2.ScopedName JSON.Value
annotationsToA2 = Map.fromList . map (\(k,(_,v)) -> (scopedNameToA2 k,v)) . Map.toList

-- | Convert the external AST into the internal one, suitable for codegen etc.

moduleFromA2 :: A2.Module -> SModule
moduleFromA2 m = Module {
  m_name = moduleNameFromA2 (A2.module_name m),
  m_imports = map importFromA2 (A2.module_imports m),
  m_decls = Map.fromList [(d_name d, d) | d <- decls],
  m_declOrder = [d_name d | d <- decls],
  m_annotations = annotationsFromA2 (A2.module_annotations m)
  }
  where
    decls = map declFromA2 (SM.elems (A2.module_decls m))

moduleNameFromA2 :: A2.ModuleName -> ModuleName 
moduleNameFromA2 = ModuleName . T.splitOn "."

importFromA2 :: A2.Import -> Import
importFromA2 (A2.Import_moduleName mn) = Import_Module (moduleNameFromA2 mn)
importFromA2 (A2.Import_scopedName sn) = Import_ScopedName (scopedNameFromA2 sn)

declFromA2 :: A2.Decl -> SDecl
declFromA2 decl = Decl {
  d_name  = A2.decl_name decl,
  d_version = fmap fromIntegral (A2.decl_version decl),
  d_annotations = annotationsFromA2 (A2.decl_annotations decl),
  d_type = declTypeFromA2 (A2.decl_type_ decl),
  d_customType = ()
}

scopedNameFromA2 :: A2.ScopedName -> ScopedName
scopedNameFromA2 sn = ScopedName {
  sn_moduleName = moduleNameFromA2 (A2.scopedName_moduleName sn),
  sn_name = A2.scopedName_name sn
}

annotationsFromA2 :: A2.Annotations -> Annotations ScopedName
annotationsFromA2 = Map.fromList . map conv . Map.toList
  where
    conv (sn,jv) = let sn' = scopedNameFromA2 sn in (sn', (sn', jv))

declTypeFromA2 :: A2.DeclType -> DeclType ScopedName
declTypeFromA2 (A2.DeclType_struct_ s) = Decl_Struct $ Struct {
  s_typeParams = A2.struct_typeParams s,
  s_fields = map fieldFromA2 (A2.struct_fields s)
}
declTypeFromA2 (A2.DeclType_union_ u) = Decl_Union $ Union {
  u_typeParams = A2.union_typeParams u,
  u_fields = map fieldFromA2 (A2.union_fields u)
}
declTypeFromA2 (A2.DeclType_type_ t) = Decl_Typedef $ Typedef {
  t_typeParams = A2.typeDef_typeParams t,
  t_typeExpr =  typeExprFromA2 (A2.typeDef_typeExpr t)
}

declTypeFromA2 (A2.DeclType_newtype_ n) = Decl_Newtype $ Newtype {
  n_typeParams = A2.newType_typeParams n,
  n_typeExpr =  typeExprFromA2 (A2.newType_typeExpr n),
  n_default = A2.newType_default n
}

fieldFromA2 :: A2.Field -> Field ScopedName
fieldFromA2 f = Field {
  f_name = A2.field_name f,
  f_serializedName = A2.field_serializedName f,
  f_type = typeExprFromA2 (A2.field_typeExpr f),
  f_default = A2.field_default f,
  f_annotations = annotationsFromA2 (A2.field_annotations f)
}

typeExprFromA2 :: A2.TypeExpr -> TypeExpr ScopedName
typeExprFromA2 te = TypeExpr ref params 
  where
    ref = scopedNameFromTypeRefA2 (A2.typeExpr_typeRef te)
    params = map typeExprFromA2 (A2.typeExpr_parameters te)

scopedNameFromTypeRefA2 :: A2.TypeRef -> ScopedName
scopedNameFromTypeRefA2 (A2.TypeRef_primitive ident) = scopedNameFromIdent ident
scopedNameFromTypeRefA2 (A2.TypeRef_typeParam ident) = scopedNameFromIdent ident
scopedNameFromTypeRefA2 (A2.TypeRef_reference sn) = scopedNameFromA2 sn

scopedNameFromIdent :: Ident -> ScopedName
scopedNameFromIdent ident = ScopedName (ModuleName []) ident