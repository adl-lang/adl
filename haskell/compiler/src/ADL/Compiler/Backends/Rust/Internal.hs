{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Rust.Internal where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Text as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as C
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import qualified Data.Aeson as JSON

import ADL.Compiler.AST
import ADL.Compiler.Primitive
import ADL.Compiler.Processing
import ADL.Utils.Format(template,formatText)
import ADL.Utils.IndentedCode
import Control.Applicative
import Control.Monad(when)
import Control.Monad.Trans.State.Strict
import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.Scientific(isInteger)
import System.FilePath(joinPath)

-- | Command line flags to control the backend.
-- (once we have them)
data RustFlags = RustFlags {
  rsLibDir :: FilePath,
  rsIncludeRuntime :: Bool,
  rsRuntimeDir :: FilePath
}

data CodeGenProfile = CodeGenProfile {
}

-- Currently we don't support custom types, but when we do,
-- they would go here (see the java backend as an example)

data CustomType = CustomType
  deriving (Show)

-- We use a state monad to accumulate details of the rust file
-- corresponding to each ADL module
type CState a = State ModuleFile a

data ModuleFile = ModuleFile {
  mfModuleName   :: ModuleName,

  -- The imports upon which this module depends
  mfImports      :: M.Map Ident RSImport,

  -- The code
  mfDeclarations :: [Code],

  -- Details to control the code generate
  mfCodeGenProfile :: CodeGenProfile
}

data RSImport = RSImport {
} deriving (Eq, Show, Ord)

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

genModuleCode :: T.Text -> ModuleFile -> LBS.ByteString
genModuleCode cmd mf = genCode code
  where
    code
      =  ctemplate "// $1generated from adl module $2" ["@", formatText (mfModuleName mf)]
      <> cline ""
      <> mconcat [genImport (mfModuleName mf) i | i <- M.elems (mfImports mf)]
      <> cline ""
      <> mconcat (L.intersperse (cline "") (reverse (mfDeclarations mf)))

    genCode code = LBS.fromStrict (T.encodeUtf8 (T.unlines (codeText Nothing code)))

genImport :: ModuleName -> RSImport -> Code
genImport intoModule RSImport{} = mempty

generateCode :: Annotations t -> Bool
generateCode annotations = case M.lookup snRustGenerate annotations of
  Just (_,JSON.Bool gen) -> gen
  _ -> True

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType _ _ = Nothing

emptyModuleFile :: ModuleName -> CodeGenProfile -> ModuleFile
emptyModuleFile mn cgp = ModuleFile mn M.empty [] cgp

moduleFilePath  :: [Ident] -> FilePath
moduleFilePath path = joinPath (map T.unpack path)

snRustGenerate :: ScopedName
snRustGenerate = ScopedName (ModuleName ["adlc","config","rust"]) "RustGenerate"
