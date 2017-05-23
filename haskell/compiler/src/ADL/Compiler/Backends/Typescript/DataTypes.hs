{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.DataTypes where

import           ADL.Compiler.AST                 (Decl, Module, ModuleName,
                                                   TypeExpr, Ident, Field, Annotations, Struct)
import           ADL.Compiler.Processing          (ResolvedTypeT)
import           Control.Monad.Trans.State.Strict (State)
import qualified Data.Map                         as Map
import           ADL.Utils.IndentedCode (Code)
import qualified Data.Text                        as T (Text)

-- | Command line flags to control the backend.
-- (once we have them)
data TypescriptFlags = TypescriptFlags {
  tsLibDir :: FilePath,
  tsIncludeRuntime :: Bool,
  tsRuntimeDir :: FilePath
}

-- A variant of the AST that carries custom type
-- information. A `CModule` value is the input to
-- our code generation process

type CModule = Module (Maybe CustomType) CResolvedType
type CResolvedType = ResolvedTypeT (Maybe CustomType)
type CTypeExpr = TypeExpr CResolvedType
type CDecl = Decl (Maybe CustomType) CResolvedType
type CAnnotations = Annotations CResolvedType
type CStruct = Struct CResolvedType
type CField = Field CResolvedType

type InterfaceName = T.Text
type ParameterNames = [Ident]

-- ... but currently we don't support custom types, but when we do,
-- they would go here (see the java backend as an example)

data CustomType = CustomType
  deriving (Show)

-- We use a state monad to accumulate details of the typescript file
-- corresponding to each ADL module
type CState a = State ModuleFile a

data ModuleFile = ModuleFile {
  mfModuleName   :: ModuleName,

  -- The imports upon which this module depends
  mfImports      :: Map.Map Ident TSImport,

  -- The code
  mfDeclarations :: [Code]
}

data TSImport = TSImport {
  iAsName       :: Ident,
  iModulePath :: [Ident]
} deriving (Eq, Show, Ord)

-- data structure to capture all of the details
-- we need for a field

data FieldDetails = FieldDetails {
  fdField       :: Field CResolvedType,
  fdName        :: T.Text,
  fdTypeExprStr :: T.Text,
  fdOptional    :: Bool,
  fdDefValue    :: Maybe Code
};

instance Eq FieldDetails where
  fd1 == fd2 = fdName fd1 == fdName fd2

instance Ord FieldDetails where
  fd1 `compare` fd2 = fdName fd1 `compare` fdName fd2

-- | The key functions needed to plug a type into the
-- code generator
newtype TypeDetails = TypeDetails {
  -- | Generate the json representation of the type,
  -- given the representation of the type arguments.
  tdType :: [T.Text] -> CState T.Text
}
