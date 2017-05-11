{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript(
 generate,
  TypescriptFlags(..),
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Encoding as T

import Control.Monad(when)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(State,execState,modify,get,put)
import Data.Aeson( (.=) )
import Data.List(intersperse)
import Data.Monoid
import Data.Foldable(for_)
import System.FilePath(joinPath,(</>),(<.>))

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.DataFiles
import ADL.Compiler.Primitive
import ADL.Compiler.Processing
import ADL.Compiler.Utils
import ADL.Utils.FileDiff(dirContents)
import ADL.Utils.Format
import ADL.Utils.IndentedCode

-- | Command line flags to control the backend.
-- (once we have them)

data TypescriptFlags = TypescriptFlags

-- A variant of the AST that carries custom type
-- information. A `CModule` value is the input to
-- our code generation process

type CModule = Module (Maybe CustomType) CResolvedType
type CResolvedType = ResolvedTypeT (Maybe CustomType)
type CTypeExpr = TypeExpr (CResolvedType)
type CDecl = Decl (Maybe CustomType) CResolvedType

-- ... but currently we don't support custom types, but when we do,
-- they would go here (see the java backend as an example)
  
data CustomType = CustomType
  deriving (Show)

-- We use a state monad to accumulate details of the typescript file
-- corresponding to each ADL module
type CState a = State ModuleFile a

data ModuleFile = ModuleFile {
  mf_moduleName :: ModuleName,
  
  -- The imports upon which this module depends
  mf_imports :: Map.Map ScopedName TSImport,

  -- The code
  mf_declarations :: [Code]
}

data TSModuleName = TSModuleName {
  mn_package :: [Ident],
  mn_name :: Ident
} deriving (Eq,Ord,Show)

data TSImport = TSImport {
  i_name :: Ident,
  i_asName :: Ident,
  i_fromModule :: TSModuleName
}

-- | Run this backend on a list of ADL modules. Check each module
-- for validity, and then generate the code for it.
generate :: AdlFlags -> TypescriptFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af tf fileWriter modulePaths = catchAllExceptions  $ do
    for_ modulePaths $ \modulePath -> do
      m <- loadAndCheckModule af modulePath
      generateModule tf fileWriter m

-- | Generate and the typescript code for a single ADL module, and
-- save the resulting code to the apppropriate file      
generateModule :: TypescriptFlags ->
                  FileWriter ->
                  RModule ->
                  EIO T.Text ()
generateModule tf fileWriter m0 = do
  let moduleName = m_name m
      m = associateCustomTypes getCustomType moduleName m0
      decls = Map.elems (m_decls m)
      mf = execState (genModule m) (emptyModuleFile (m_name m))
  liftIO $ fileWriter (moduleFilePath (tsModuleName moduleName)) (genModuleCode mf) 

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType scopedName decl = Nothing

emptyModuleFile :: ModuleName -> ModuleFile
emptyModuleFile mn = ModuleFile mn Map.empty []

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mf_declarations=code:mf_declarations mf})

addImport :: ScopedName -> TSImport -> CState ()
addImport sn i = modify (\mf->mf{mf_imports=Map.insert sn i (mf_imports mf)})

moduleFilePath  :: TSModuleName -> FilePath
moduleFilePath (TSModuleName path name) = joinPath (map T.unpack path) </> T.unpack name <.> "ts"

-- | Convert an ADL module name to a typescript one
tsModuleName :: ModuleName -> TSModuleName
tsModuleName (ModuleName ids) = TSModuleName (reverse rest) last
  where
    (last:rest) = reverse ids
    
genModule :: CModule -> CState ()
genModule m = do
  -- Generate each declaration
  for_ (Map.elems (m_decls m)) $ \decl -> do
    case d_type decl of
     (Decl_Struct struct) -> genStruct m decl struct
     (Decl_Union union) -> genUnion m decl union
     (Decl_Typedef typedef) -> genTypedef m decl typedef
     (Decl_Newtype ntype) -> genNewtype m decl ntype

genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct  m decl struct = do
  addDeclaration placeholder
  where
    placeholder = ctemplate "// FIXME: struct for $1 to be implemented" [d_name decl]

genUnion :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnion  m decl  union = do
  addDeclaration placeholder
  where
    placeholder = ctemplate "// FIXME: union for $1 to be implemented" [d_name decl]

genTypedef :: CModule -> CDecl -> Typedef CResolvedType -> CState ()
genTypedef  m decl  typedef = do
  addDeclaration placeholder
  where
    placeholder = ctemplate "// FIXME: typedef for $1 to be implemented" [d_name decl]

genNewtype :: CModule -> CDecl -> Newtype CResolvedType -> CState ()
genNewtype  m decl  ntype = do
  addDeclaration placeholder
  where
    placeholder = ctemplate "// FIXME: newtype for $1 to be implemented" [d_name decl]

genModuleCode :: ModuleFile -> LBS.ByteString
genModuleCode mf = LBS.fromStrict (T.encodeUtf8 (T.unlines (codeText 10000 code)))
  where
    code
      =  mconcat [genImport i | i <- Map.elems (mf_imports mf)]
      <> cline ""
      <> mconcat (intersperse (cline "") (mf_declarations mf))
      
    genImport :: TSImport -> Code
    genImport i
      | i_asName i == i_name i = ctemplate "import { $1 } from \"$2\"" [i_name i,mpath]
      | otherwise              = ctemplate "import { $1 as $2 } from \"$2\"" [i_name i, i_asName i, mpath]
      where
        mpath = T.intercalate "/" (mn_package mn <> [mn_name mn])
        mn = i_fromModule i
