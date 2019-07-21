{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Rust.Internal where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Text as JS
import qualified Data.ByteString.Lazy as LBS
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
  iAsName       :: Ident,
  iModulePath :: [Ident]
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

data FieldDetails = FieldDetails {
  fdField       :: Field CResolvedType,
  fdName        :: T.Text,
  fdTypeExprStr :: T.Text,
  fdOptional    :: Bool,
  fdDefValue    :: Maybe T.Text
};

-- | The key functions needed to plug a type into the
-- code generator
data TypeDetails = TypeDetails {
  -- | Generate the text representation of the typescript type,
  -- given the representation of the type arguments.
  --
  -- eg adl type Vector<Int32> would have representation `number[]`
  --
  td_type :: [T.Text] -> CState T.Text,

  -- | Generate a typescript literal value
  td_genLiteralText :: Literal CTypeExpr -> CState T.Text,

  -- | Generate the expression to create an AST type value,
  -- given the representation of the type arguments.
  --
  -- eg adl type Vector<Int32> would have a type value expr `texprVector(texprInt32())`
  --
  td_typeValue :: [T.Text] -> CState T.Text
}

genModuleCode :: T.Text -> ModuleFile -> LBS.ByteString
genModuleCode cmd mf = genCode code
  where
    code
      =  ctemplate "// $1generated from adl module $2" ["@", formatText (mfModuleName mf)]
      <> ctemplate "pub mod $1 {" [formatText (mfModuleName mf)]
      <> mconcat [genImport (mfModuleName mf) i | i <- M.elems (mfImports mf)]
      <> cline ""
      <> mconcat (L.intersperse (cline "") (reverse (mfDeclarations mf)))
      <> cline "}"

    genCode code = LBS.fromStrict (T.encodeUtf8 (T.unlines (codeText Nothing code)))

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mfDeclarations=code:mfDeclarations mf})

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails field = do
  let te = f_type field
      isBoxed = M.member snRustBoxed (f_annotations field)
  typeExprStr0 <- genTypeExpr te
  let typeExprStr = case isBoxed of
        False -> typeExprStr0
        True -> template "Box<$1>" [typeExprStr0]
  defValueStr <- case f_default field of
    (Just v) -> case literalForTypeExpr te v of
      Left e -> error ("BUG: invalid json literal: " ++ T.unpack e)
      Right litv -> fmap Just (genLiteralText litv)
    Nothing -> return Nothing
  return (FieldDetails field (f_name field) typeExprStr False defValueStr)

-- | Generate the typescript type given an ADL type expression
genTypeExpr :: CTypeExpr -> CState T.Text
genTypeExpr (TypeExpr rt params) = do
  rtParamsStr <- mapM genTypeExpr  params
  td_type (getTypeDetails rt) rtParamsStr

-- | Generate the typescript expr to creat an AST type value
genTypeValueExpr :: CTypeExpr -> CState T.Text
genTypeValueExpr (TypeExpr rt params) = do
  rtParamsStr <- mapM genTypeValueExpr  params
  td_typeValue (getTypeDetails rt) rtParamsStr

-- | Generate an expression to construct a literal value.
genLiteralText :: Literal CTypeExpr -> CState T.Text
genLiteralText lit@(Literal (TypeExpr rt _) _) = td_genLiteralText (getTypeDetails rt) lit

-- | Get the TypeDetails record for any resolved type.
getTypeDetails :: CResolvedType -> TypeDetails

-- each primitive
getTypeDetails (RT_Primitive pt) =
  case pt of
    P_String -> primTypeDetails "String" toString
    P_Double -> primTypeDetails "f64" toNumber
    P_Float -> primTypeDetails "f32" toNumber
    P_Int8 -> primTypeDetails "i8" toNumber
    P_Int16 -> primTypeDetails "i16" toNumber
    P_Int32 -> primTypeDetails "i32" toNumber
    P_Int64 -> primTypeDetails "i64" toNumber
    P_Word8 -> primTypeDetails "u8" toNumber
    P_Word16 -> primTypeDetails "u16" toNumber
    P_Word32 -> primTypeDetails "u32" toNumber
    P_Word64 -> primTypeDetails "u64" toNumber
    P_Bool -> primTypeDetails "bool" toBool
    P_Void -> primTypeDetails "null" (error "P_Void primitive not implemented")
    P_ByteVector -> primTypeDetails "Uint8Array" (error "P_ByteVector  primitive not implemented")
    P_Json -> primTypeDetails "{}|null" (error "P_Json  primitive not implemented")
    P_Vector -> vectorTypeDetails
    P_StringMap -> stringMapTypeDetails
    P_Nullable -> nullableTypeDetails
  where
    primTypeDetails t convf = TypeDetails (const (return t)) convf (const (return ("ADL.texpr" <> ptToText pt <> "()")))

    toString (Literal _ (LPrimitive (JS.String s))) = return (T.pack (show s))
    toString _ = error "BUG: expected a string literal"

    toNumber (Literal _ (LPrimitive (JS.Number n))) = return (litNumber n)
    toNumber _ = error "BUG: expected a number literal"

    toBool (Literal _ (LPrimitive (JS.Bool True))) = return "true"
    toBool (Literal _ (LPrimitive (JS.Bool False))) = return "false"
    toBool _ = error "BUG: expected a boolean literal"


    vectorTypeDetails = TypeDetails typeExpr literalText typeValue
      where
        typeExpr [texpr] = return ("Vec<" <> texpr <> ">")
        typeExpr _ = error "BUG: expected a single type param for Vector"
        literalText (Literal te (LVector ls)) = do
          lits <- mapM genLiteralText ls
          return (template "vec![$1]" [T.intercalate ", " lits])
        literalText _ = error "BUG: invalid literal for Vector"
        typeValue [tvalue] = return (template "ADL.texprVector($1)" [tvalue])
        typeValue _ = error "BUG: expected a single type param for Vector"

    stringMapTypeDetails = error "stringmap not implemented"
    -- stringMapTypeDetails = TypeDetails typeExpr literalText typeValue
    --   where
    --     typeExpr [texpr] = return (template "{[key: string]: $1}" [texpr])
    --     typeExpr _ = error "BUG: expected a single type param for StringMap"
    --     literalText (Literal _ (LStringMap m)) = do
    --       m' <- traverse genLiteralText m
    --       return (template "{$1}" [T.intercalate ", " [ template "$1 : $2" [k,v] | (k,v) <- M.toList m']])
    --     literalText _ = error "BUG: invalid literal for StringMap"
    --     typeValue [tvalue] = return (template "ADL.texprStringMap($1)" [tvalue])
    --     typeValue _ = error "BUG: expected a single type param for StringMap"

    nullableTypeDetails = error "nullable not implemented"
    --nullableTypeDetails = TypeDetails typeExpr literalText typeValue
    --  where
    --    typeExpr [texpr] = return (template "($1|null)" [texpr])
    --    typeExpr _ = error "BUG: expected a single type param for StringMap"
    --    literalText (Literal _ (LNullable Nothing)) = return "null"
    --    literalText (Literal _ (LNullable (Just l))) = genLiteralText l
    --    literalText _ = error "BUG: invalid literal for Nullable"
    --    typeValue [tvalue] = return (template "ADL.texprNullable($1)" [tvalue])
    --    typeValue _ = error "BUG: expected a single type param for Nullable"

-- a type defined through a regular ADL declaration
getTypeDetails rt@(RT_Named (scopedName,Decl{d_customType=Nothing})) = TypeDetails typeExpr literalText typeValue
  where
    (ScopedName moduleName name) = scopedName
    typeExpr typeArgs = do
      currentModuleName <- fmap mfModuleName get
      let modules =  if moduleName == currentModuleName then [] else unModuleName moduleName
      addModulesImport modules name
      return (modulePrefix modules <> name <> typeParamsExpr typeArgs)
    literalText (Literal te LDefault) = error "BUG: literal defaults shouldn't be needed"
    literalText (Literal (TypeExpr (RT_Named (_, Decl{d_type=Decl_Struct struct})) _) (LCtor ls)) = do
      lvs <- mapM genLiteralText ls
      return (template "{$1}" [T.intercalate ", " [template "$1 : $2" [f_name f,v] | (f,v) <- zip (s_fields struct) lvs]])
    literalText (Literal (TypeExpr (RT_Named (_, Decl{d_type=Decl_Newtype _})) _) (LCtor [l])) = do
      genLiteralText l
    literalText (Literal te@(TypeExpr (RT_Named (_, Decl{d_type=Decl_Union union})) _) (LUnion ctor l)) = do
      lv <- genLiteralText l
      case te of
       te | refEnumeration te -> let (i,f) = findUnionField ctor (u_fields union)
                                 in return (T.pack (show i))
          | isVoidLiteral l -> return (template "{kind : \"$1\"}" [ctor])
          | otherwise -> return (template "{kind : \"$1\", value : $2}" [ctor,lv])
    literalText l = error ("BUG: missing RT_Named literalText definition (" <> show l <> ")")

    typeValue typeValueArgs = do
      currentModuleName <- fmap mfModuleName get
      let modules =  if moduleName == currentModuleName then [] else unModuleName moduleName
      addModulesImport modules name
      return (template "$1texpr$2($3)" [modulePrefix modules, name, T.intercalate ", " typeValueArgs])

-- a custom type
getTypeDetails rt@(RT_Named (_,Decl{d_customType=Just customType})) =
  error "BUG: custom types not implemented"

-- a type variable
getTypeDetails (RT_Param typeVar) = TypeDetails typeExpr literalText typeValue
  where
    typeExpr _ = return typeVar
    literalText _ = error "BUG: literal values for type variables shouldn't be needed"
    typeValue _ = error "BUG: type values expressions can't be created for type variables"

renderCommentForDeclaration :: CDecl -> Code
renderCommentForDeclaration decl = mconcat $ map renderDeclComment $ M.elems (d_annotations decl)
  where
    renderDeclComment :: (CResolvedType, JS.Value) -> Code
    renderDeclComment (RT_Named (ScopedName{sn_name="Doc"}, _), JS.String commentText) = renderComment commentText
    renderDeclComment _ = CEmpty

renderCommentForField :: CField -> Code
renderCommentForField field = mconcat $ map renderFieldComment $ M.elems (f_annotations field)
  where
    renderFieldComment (RT_Named (ScopedName{sn_name="Doc"}, _), JS.String commentText) = renderComment commentText
    renderFieldComment _ = CEmpty

renderComment :: T.Text -> Code
renderComment commentValue = clineN commentLinesStarred
  where
    commentLinesStarred = ["/**"] ++ [" * " <> commentLine | commentLine <- commentLinesBroken] ++ [" */"]
    commentLinesBroken = L.filter (/= "") (T.splitOn "\n" commentValue)

typeParamsExpr :: [T.Text] -> T.Text
typeParamsExpr []         = T.pack ""
typeParamsExpr parameters = "<" <> T.intercalate ", " parameters <> ">"

enumVariantName :: FieldDetails -> T.Text
enumVariantName fd = capitalise (fdName fd)

addImport :: Ident -> RSImport -> CState ()
addImport moduleIdentity tsImport = modify (\mf->mf{mfImports=M.insert moduleIdentity tsImport (mfImports mf)})

findUnionField :: T.Text -> [Field CResolvedType] -> (Int,Field CResolvedType)
findUnionField fname fs = case L.find (\(_,f) -> f_name f == fname) (zip [0,1..] fs) of
  (Just v) -> v
  Nothing -> error ("BUG: invalid literal " <> show fname <> "for union")

addModulesImport :: [Ident] -> T.Text -> CState ()
addModulesImport [] _ = return ()
addModulesImport modules _ = addImport importAsName tsImport
    where
      tsImport = RSImport{iAsName=importAsName, iModulePath=modules}
      importAsName = T.intercalate "_" modules

modulePrefix :: [Ident] -> T.Text
modulePrefix [] = T.pack ""
modulePrefix modules = T.intercalate "_" modules <> "."

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

snRustBoxed :: ScopedName
snRustBoxed = ScopedName (ModuleName ["adlc","config","rust"]) "RustBoxed"
