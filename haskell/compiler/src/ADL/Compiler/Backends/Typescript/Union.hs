{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Union where

import           ADL.Compiler.AST
import           ADL.Compiler.Backends.Typescript.Common
import           ADL.Compiler.Backends.Typescript.DataTypes
import           ADL.Compiler.Primitive                     (PrimitiveType (P_String, P_Void))
import           ADL.Compiler.Processing
import           ADL.Utils.Format                           (template)
import           ADL.Utils.IndentedCode
import qualified Data.List                                  as L (sort)
import qualified Data.Map                                   as Map (empty)
import           Data.Monoid
import qualified Data.Text                                  as T (Text,
                                                                  intercalate)

-- Generate code for union declaration.
genUnion :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnion  m decl union@Union{u_typeParams=parameters} = do
  genUnionWithDiscriminate m decl union
  addAstDeclaration m decl

genUnionWithDiscriminate :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionWithDiscriminate  m decl union
  | isUnionEnum union = genUnionEnum m decl union
  | otherwise = genUnionInterface m decl union

genUnionEnum :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionEnum _ decl enum = do
  fds <- mapM genFieldDetails (u_fields enum)
  let enumName = capitalise (d_name decl)
      enumFields = mconcat [ctemplate "$1," [fdName fd] | fd <- fds]
      enumDecl = cblock (template "export enum $1" [enumName]) enumFields
  addDeclaration enumDecl

genUnionInterface :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnionInterface _ decl union@Union{u_typeParams=parameters} = do
  fds <- mapM genFieldDetails (u_fields union)
  let unionName = d_name decl
      sortedFds = L.sort fds
  addDeclaration (renderUnionFieldsAsInterfaces unionName parameters sortedFds)
  addDeclaration (renderUnionFieldFactories unionName parameters sortedFds)
  addDeclaration (renderUnionChoice decl unionName parameters sortedFds)

renderUnionChoice :: CDecl -> T.Text -> [Ident] -> [FieldDetails] -> Code
renderUnionChoice decl unionName typeParams fds =
  CAppend renderedComments (ctemplate "export type $1$2 = $3;" [unionName, renderedParameters, T.intercalate " | " [getChoiceName fd | fd <- fds]])
  where
    getChoiceName fd = unionName <> "_" <> capitalise (fdName fd) <> renderedParameters
    renderedComments = renderCommentsForDeclaration decl
    renderedParameters = typeParamsExpr typeParams

renderUnionFieldsAsInterfaces :: T.Text -> [Ident] -> [FieldDetails] -> Code
renderUnionFieldsAsInterfaces unionName parameters (fd:xs) =
  CAppend renderedInterface (renderUnionFieldsAsInterfaces unionName parameters xs)
    where
      renderedInterface = CAppend (renderInterface interfaceName parameters fieldDetails True) CEmpty
      interfaceName = unionName <> "_" <> capitalise (fdName fd)
      fieldDetails = constructUnionFieldDetailsFromField fd
renderUnionFieldsAsInterfaces _ _ [] = CEmpty

renderUnionFieldFactories :: T.Text -> [Ident] -> [FieldDetails] -> Code
renderUnionFieldFactories unionName parameters (fd:xs) =
  CAppend renderedFactory (renderUnionFieldFactories unionName parameters xs)
    where
      renderedFactory = renderFactory interfaceName parameters fieldDetails
      interfaceName = unionName <> "_" <> capitalise (fdName fd)
      fieldDetails = constructUnionFieldDetailsFromField fd
renderUnionFieldFactories _ _ [] = CEmpty

constructUnionFieldDetailsFromField :: FieldDetails -> [FieldDetails]
constructUnionFieldDetailsFromField fd@FieldDetails{fdField=Field{f_type=(TypeExpr (RT_Primitive P_Void) _)}}
 = [FieldDetails{
  fdName="kind",
  fdField=Field{
    f_name="kind",
    f_serializedName="kind",
    f_type=TypeExpr (RT_Primitive P_String) [],
    f_default=Nothing,
    f_annotations=Map.empty},
  fdTypeExprStr="'" <> fdName fd <> "'",
  fdOptional=False,
  fdDefValue=Nothing}]
constructUnionFieldDetailsFromField fd = [FieldDetails{
  fdName="kind",
  fdField=Field{
    f_name="kind",
    f_serializedName="kind",
    f_type=TypeExpr (RT_Primitive P_String) [],
    f_default=Nothing,
    f_annotations=Map.empty},
  fdTypeExprStr="'" <> fdName fd <> "'",
  fdOptional=False,
  fdDefValue=Nothing},
  FieldDetails{fdName="value",
  fdField=Field{
    f_name="value",
    f_serializedName="value",
    f_type=TypeExpr (RT_Primitive P_String) [],
    f_default=Nothing,
    f_annotations=Map.empty},
  fdTypeExprStr=fdTypeExprStr fd,
  fdOptional=False,
  fdDefValue=Nothing}]
