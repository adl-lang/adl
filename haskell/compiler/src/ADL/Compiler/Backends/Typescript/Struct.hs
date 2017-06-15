{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Struct where

import Data.Monoid

import ADL.Compiler.AST
import ADL.Compiler.Backends.Typescript.Common
import ADL.Utils.IndentedCode
import ADL.Compiler.Backends.Typescript.DataTypes

genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct m decl struct@Struct{s_typeParams=parameters} = do
  fds <- mapM genFieldDetails (s_fields struct)
  let structName = capitalise (d_name decl)

  addDeclaration $ renderCommentsForDeclaration decl <> renderInterface structName parameters fds False
  addDeclaration $ renderFactory structName (s_typeParams struct) fds
  addAstDeclaration m decl
