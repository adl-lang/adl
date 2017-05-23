{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Newtype where
import           ADL.Compiler.AST
import           ADL.Compiler.Backends.Typescript.Common
import           ADL.Compiler.Backends.Typescript.DataTypes
import           ADL.Utils.IndentedCode


genNewtype :: CModule -> CDecl -> Newtype CResolvedType -> CState ()
genNewtype  m declaration ntype@Newtype{n_typeParams=parameters} = do
  typeExprOutput <- genTypeExpr (n_typeExpr ntype)
  let
    placeholder = ctemplate "export type $1 = $2;" [d_name declaration, typeExprOutput]
  addDeclaration $ renderTypeRef m declaration parameters
  addDeclaration placeholder
