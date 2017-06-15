{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Typedef where
import           ADL.Compiler.AST
import           ADL.Compiler.Backends.Typescript.Common
import           ADL.Compiler.Backends.Typescript.DataTypes
import           ADL.Utils.IndentedCode

genTypedef :: CModule -> CDecl -> Typedef CResolvedType -> CState ()
genTypedef m declaration typedef@Typedef{t_typeParams=typeParams} = do
  typeExprOutput <- genTypeExpr (t_typeExpr typedef)
  let
    typeDecl = ctemplate "export type $1$2 = $3;" [d_name declaration, typeParamsExpr typeParams, typeExprOutput]
  addDeclaration typeDecl
  addAstDeclaration m declaration
