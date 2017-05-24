{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Struct where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Text as JS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V

import Data.Monoid
import Data.Scientific(isInteger)

import ADL.Compiler.AST
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Backends.Typescript.Common
import ADL.Utils.IndentedCode
import ADL.Compiler.Backends.Typescript.DataTypes

genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct m decl struct@Struct{s_typeParams=parameters} = do
  fds <- mapM genFieldDetails (s_fields struct)
  let structName = capitalise (d_name decl)
  let astDecl
        =  ctemplate "const $1_AST =" [structName]
        <> indent (ctemplate "$1;" [jsonToText (structAst struct)])

  addDeclaration $ renderCommentsForDeclaration decl <> renderInterface structName parameters fds False
  addDeclaration $ renderFactory structName (s_typeParams struct) fds
  addAstDeclaration astDecl

structAst :: Struct CResolvedType -> JS.Value
structAst s = JS.object
  [ ("fields", JS.toJSON (map fieldAst (s_fields s)))
  , ("typeParams", JS.toJSON (s_typeParams s))
  ]

fieldAst :: Field CResolvedType -> JS.Value
fieldAst f = JS.object
 [ ("name", JS.toJSON (f_name f))
 , ("serializedName", JS.toJSON (f_serializedName f))
 , ("type", typeExprAst (f_type f))
 , ("default", mkMaybe (fmap literalAst (f_default f)))
 , ("annotations", annotationsAst (f_annotations f))
 ]

annotationsAst :: Annotations CResolvedType -> JS.Value
annotationsAst a = JS.toJSON ([]::[()]) -- FIXME and implement

typeExprAst :: TypeExpr CResolvedType -> JS.Value
typeExprAst (TypeExpr tr tes) = JS.object
 [ ("typeRef", typeRefAst tr)
 , ("parameters", JS.toJSON (map typeExprAst tes))
 ]

typeRefAst :: CResolvedType -> JS.Value
typeRefAst (RT_Named (sn,_)) = mkUnion "reference" (scopedNameAst sn)
typeRefAst (RT_Param id) = mkUnion "typeParam" (JS.toJSON id)
typeRefAst (RT_Primitive p) = mkUnion "primitive" (JS.toJSON (ptToText p))

scopedNameAst :: ScopedName -> JS.Value
scopedNameAst (ScopedName moduleName name) = JS.object
  [ ("moduleName", moduleNameAst moduleName)
  , ("name", JS.toJSON name)
  ]

moduleNameAst :: ModuleName -> JS.Value
moduleNameAst (ModuleName mn) = JS.toJSON (T.intercalate "." mn)

literalAst :: JS.Value -> JS.Value
literalAst (JS.Array jvs) = mkUnion "array" (JS.toJSON (map literalAst (V.toList jvs)))
literalAst (JS.Bool b) = mkUnion "boolean" (JS.toJSON b)
literalAst (JS.Number v) | isInteger v = mkUnion "integer" (JS.toJSON v) -- Should git rid of this distinction in adlast.adl
                          | otherwise = mkUnion "double" (JS.toJSON v)
literalAst (JS.Null) = mkVoidUnion "null"
literalAst o@(JS.Object _) = mkUnion "object" o
literalAst (JS.String s) = mkUnion "string" (JS.toJSON s)

mkMaybe :: Maybe JS.Value -> JS.Value
mkMaybe Nothing = JS.object [("kind", (JS.toJSON ("nothing"::T.Text)))]
mkMaybe (Just jv) = JS.object
  [ ("kind", (JS.toJSON ("just"::T.Text)))
  , ("value", jv)
  ]

mkUnion :: T.Text -> JS.Value -> JS.Value
mkUnion kind value = JS.object
  [ ("kind", (JS.toJSON kind))
  , ("value", value)
  ]

mkVoidUnion :: T.Text -> JS.Value
mkVoidUnion kind = JS.object
  [ ("kind", (JS.toJSON kind))
  ]

jsonToText :: JS.Value -> T.Text
jsonToText = LT.toStrict . JS.encodeToLazyText
