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
genStruct m decl struct@Struct{s_typeParams=parameters} = do
  fds <- mapM genFieldDetails (s_fields struct)
  let structName = capitalise (d_name decl)
      renderedComments = renderCommentsForDeclaration decl
  addDeclaration $ renderTypeRef m decl parameters
  addDeclaration $ renderFactory structName (s_typeParams struct) fds
  addDeclaration $ CAppend renderedComments (renderInterface structName parameters fds False)
