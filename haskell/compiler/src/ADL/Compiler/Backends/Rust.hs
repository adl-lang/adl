{-|
Module : ADL.Compiler.Backends.Rust
Description: Rust backend for ADL

This module contains that necessary functions to generate
a rust backend from an ADL file.
-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Rust(
 generate,
  RustFlags(..),
  ) where

import           ADL.Compiler.AST
import           ADL.Compiler.Primitive
import           ADL.Utils.FileDiff                         (dirContents)
import           ADL.Utils.Format(template,formatText)
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Map                                   as M
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as T

import           ADL.Compiler.EIO
import           ADL.Compiler.Processing
import           ADL.Compiler.Utils
import           ADL.Utils.IndentedCode
import           Control.Monad                              (when)
import           Control.Monad.Trans                        (liftIO)
import           Control.Monad.Trans.State.Strict
import           Data.Foldable                              (for_)
import           Data.List                                  (sortOn)
import           Data.Monoid
import           Data.Traversable                           (for)
import           System.FilePath                            (joinPath,
                                                             takeDirectory,
                                                             (<.>), (</>))

import           ADL.Compiler.Backends.Rust.Internal
import           ADL.Compiler.DataFiles

-- | Run this backend on a list of ADL modules. Check each module
-- for validity, and then generate the code for it.
generate :: AdlFlags -> RustFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af tf fileWriter modulePaths = catchAllExceptions  $ do
  ms <- for modulePaths $ \modulePath -> do
    m <- loadAndCheckModule af modulePath
    let m' = fullyScopedModule m
    when (generateCode (m_annotations m')) (generateModule tf fileWriter m')
    return m'
  when (rsIncludeRuntime tf) (generateRuntime af tf fileWriter modulePaths)

-- JS.generate af (JS.JavascriptFlags {}) fileWriter
generateRuntime :: AdlFlags -> RustFlags -> FileWriter -> [FilePath] -> EIOT ()
generateRuntime af tf fileWriter modulePaths = do
  return ()

-- | Generate and the rust code for a single ADL module, and
-- save the resulting code to the apppropriate file
generateModule :: RustFlags ->
                  FileWriter ->
                  RModule ->
                  EIO T.Text ()
generateModule tf fileWriter m0 = do
  let moduleName = m_name m
      m = associateCustomTypes getCustomType moduleName m0
      cgp = CodeGenProfile {}
      mf = execState (genModule m) (emptyModuleFile (m_name m) cgp)
  liftIO $ fileWriter (moduleFilePath (unModuleName moduleName) <.> "rs") (genModuleCode "adlc" mf)

genModule :: CModule -> CState ()
genModule m = do
  -- Generate each declaration
  for_ (getOrderedDecls m) $ \decl ->
    when (generateCode (d_annotations decl)) $
      case d_type decl of
        (Decl_Struct struct)   -> genStruct m decl struct
        (Decl_Union union)     -> genUnion m decl union
        (Decl_Typedef typedef) -> return ()
        (Decl_Newtype ntype)   -> return ()

genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct m decl struct@Struct{s_typeParams=typeParams} = do
  fds <- mapM genFieldDetails (s_fields struct)
  let structName = capitalise (d_name decl)
  addDeclaration $ renderCommentForDeclaration decl <> render structName typeParams fds
  where
    render :: T.Text -> [Ident] -> [FieldDetails] -> Code
    render name typeParams fields =
      cblock (template "pub struct $1$2" [name, typeParamsExpr typeParams]) renderedFields
      where
        renderedFields = mconcat [renderCommentForField (fdField fd) <> renderFieldDeclaration fd ","| fd <- fields]
    
        renderFieldDeclaration :: FieldDetails -> T.Text -> Code
        renderFieldDeclaration fd endChar
          | fdOptional fd = ctemplate "$1?: $2$3" [fdName fd, fdTypeExprStr fd, endChar]
          | otherwise = ctemplate "$1: $2$3" [fdName fd, fdTypeExprStr fd, endChar]

genUnion :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnion m decl union@Union{u_typeParams=typeParams} = do
  fds <- mapM genFieldDetails (u_fields union)
  let structName = capitalise (d_name decl)
  addDeclaration $ renderCommentForDeclaration decl <> render structName typeParams fds
  where
    render :: T.Text -> [Ident] -> [FieldDetails] -> Code
    render name typeParams fields =
      cblock (template "pub enum $1$2" [name, typeParamsExpr typeParams]) renderedFields
      where
        renderedFields = mconcat [renderCommentForField (fdField fd) <> renderFieldDeclaration fd | fd <- fields]
    
        renderFieldDeclaration :: FieldDetails -> Code
        renderFieldDeclaration fd 
          | otherwise = ctemplate "$1($2)," [enumVariantName fd, fdTypeExprStr fd]
