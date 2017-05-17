{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Struct where

import           ADL.Compiler.AST
import           ADL.Compiler.Backends.Typescript.Common
import           ADL.Utils.IndentedCode

import           ADL.Compiler.Backends.Typescript.DataTypes (CDecl, CModule,
                                                             CResolvedType,
                                                             CState)

genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct _ decl struct = do
  fds <- mapM genFieldDetails (s_fields struct)
  let structName = capitalise (d_name decl)
      renderedComments = renderCommentsForDeclaration decl
  addDeclaration $ CAppend renderedComments (renderInterface structName (s_typeParams struct) fds False)
  addDeclaration $ renderFactory structName (s_typeParams struct) fds