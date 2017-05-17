{-|
Module : ADL.Compiler.Backends.Typescript
Description: Typescript backend for ADL

This module contains that necessary functions to generate
a typescript backend from an ADL file.
-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript(
 generate,
  TypescriptFlags(..),
  ) where

import           ADL.Compiler.AST
import           ADL.Compiler.Backends.Typescript.DataTypes
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Map                                   as Map
import qualified Data.Text                                  as T
import qualified Data.Text.Encoding                         as T

import           ADL.Compiler.EIO
import           ADL.Compiler.Processing
import           ADL.Compiler.Utils
import           ADL.Utils.IndentedCode
import           Control.Monad.Trans                        (liftIO)
import           Control.Monad.Trans.State.Strict
import           Data.Foldable                              (for_)
import           Data.List                                  (intersperse)
import           Data.Monoid
import           System.FilePath                            (joinPath,
                                                             takeDirectory,
                                                             (<.>), (</>))

import           ADL.Compiler.Backends.Typescript.Newtype   (genNewtype)
import           ADL.Compiler.Backends.Typescript.Struct    (genStruct)
import           ADL.Compiler.Backends.Typescript.Typedef   (genTypedef)
import           ADL.Compiler.Backends.Typescript.Union     (genUnion)

-- | Run this backend on a list of ADL modules. Check each module
-- for validity, and then generate the code for it.
generate :: AdlFlags -> TypescriptFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af tf fileWriter modulePaths = catchAllExceptions  $
    for_ modulePaths $ \modulePath -> do
      m <- loadAndCheckModule af modulePath
      generateModule tf fileWriter m

-- | Generate and the typescript code for a single ADL module, and
-- save the resulting code to the apppropriate file
generateModule :: TypescriptFlags ->
                  FileWriter ->
                  RModule ->
                  EIO T.Text ()
generateModule _ fileWriter m0 = do
  let moduleName = m_name m
      m = associateCustomTypes getCustomType moduleName m0
      mf = execState (genModule m) (emptyModuleFile (m_name m))
  liftIO $ fileWriter (moduleFilePath (unModuleName moduleName) <.> "ts") (genModuleCode mf)

genModule :: CModule -> CState ()
genModule m =
  -- Generate each declaration
  for_ (Map.elems (m_decls m)) $ \decl ->
    case d_type decl of
     (Decl_Struct struct)   -> genStruct m decl struct
     (Decl_Union union)     -> genUnion m decl union
     (Decl_Typedef typedef) -> genTypedef m decl typedef
     (Decl_Newtype ntype)   -> genNewtype m decl ntype

genModuleCode :: ModuleFile -> LBS.ByteString
genModuleCode mf = LBS.fromStrict (T.encodeUtf8 (T.unlines (codeText 10000 code)))
  where
    code
      =  mconcat [genImport i | i <- Map.elems (mfImports mf)]
      <> cline ""
      <> mconcat (intersperse (cline "") (mfDeclarations mf))

genImport :: TSImport -> Code
genImport TSImport{iAsName=asName, iModulePath=modulePath} = ctemplate "import * as $1 from \'$2\';" [asName, mpath]
  where
    mpath = T.pack $ takeDirectory "." </> moduleFilePath modulePath

emptyModuleFile :: ModuleName -> ModuleFile
emptyModuleFile mn = ModuleFile mn Map.empty []

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType _ _ = Nothing

moduleFilePath  :: [Ident] -> FilePath
moduleFilePath path = joinPath (map T.unpack path)

