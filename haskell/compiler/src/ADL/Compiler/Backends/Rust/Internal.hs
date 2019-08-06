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
import ADL.Utils.Format(template,formatText, fshow)
import ADL.Utils.IndentedCode
import Cases(camelize, snakify)
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
  -- The rust module into which we generate the ADL
  -- relative to the crate root
  rsModule :: RustScopedName,

  -- The absolute rust module path containing the runtime
  rsRuntimeModule :: RustScopedName
}

data RustScopedName = RustScopedName {unRustScopedName :: [Ident]}
  deriving (Eq, Ord)

type RustModuleFn = ModuleName -> RustScopedName

rustScopedName :: T.Text -> RustScopedName
rustScopedName s = RustScopedName (T.splitOn "::" s)

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

  -- The use references for this module
  mfUserRefs     :: M.Map Ident RustScopedName,

  -- The code
  mfDeclarations :: [Code],

  -- Function mappying ADL modules to rust modules
  mfRustModuleFn :: RustModuleFn,

  -- Details to control the code generate
  mfCodeGenProfile :: CodeGenProfile
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

data FieldDetails = FieldDetails {
  fdField       :: Field CResolvedType,
  fdTypeExprStr :: T.Text,
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
  td_genLiteralText :: Literal CTypeExpr -> CState T.Text

  -- | Generate the expression to create an AST type value,
  -- given the representation of the type arguments.
  --
  -- eg adl type Vector<Int32> would have a type value expr `texprVector(texprInt32())`
  --
  -- td_typeValue :: [T.Text] -> CState T.Text
}

genModuleCode :: T.Text -> ModuleFile -> LBS.ByteString
genModuleCode cmd mf = genCode code
  where
    code
      =  ctemplate "// $1generated from adl module $2" ["@", formatText (mfModuleName mf)]
      <> (if M.null (mfUserRefs mf) then mempty else cline "")
      <> mconcat [genUse shortName rsname | (shortName,rsname) <- M.toList (mfUserRefs mf)]
      <> cline ""
      <> mconcat (L.intersperse (cline "") (reverse (mfDeclarations mf)))

    genCode code = LBS.fromStrict (T.encodeUtf8 (T.unlines (codeText Nothing code)))

    genUse shortName rsname =
      if shortName == last (unRustScopedName rsname)
        then ctemplate "use $1;" [scopedName]
        else ctemplate "use $1 as $2;" [scopedName, shortName]
      where
        scopedName = T.intercalate "::" (unRustScopedName rsname)

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
  return (FieldDetails field typeExprStr defValueStr)

-- | Generate the typescript type given an ADL type expression
genTypeExpr :: CTypeExpr -> CState T.Text
genTypeExpr (TypeExpr rt params) = do
  rtParamsStr <- mapM genTypeExpr  params
  td_type (getTypeDetails rt) rtParamsStr

-- | Generate the typescript expr to creat an AST type value
-- genTypeValueExpr :: CTypeExpr -> CState T.Text
-- genTypeValueExpr (TypeExpr rt params) = do
--   rtParamsStr <- mapM genTypeValueExpr  params
--   td_typeValue (getTypeDetails rt) rtParamsStr

-- | Generate an expression to construct a literal value.
genLiteralText :: Literal CTypeExpr -> CState T.Text
genLiteralText lit@(Literal (TypeExpr rt _) _) = td_genLiteralText (getTypeDetails rt) lit

-- | Get the TypeDetails record for any resolved type.
getTypeDetails :: CResolvedType -> TypeDetails

-- each primitive
getTypeDetails (RT_Primitive pt) =
  case pt of
    P_String -> primTypeDetails "String" toString
    P_Double -> numTypeDetails "f64"
    P_Float -> numTypeDetails "f32"
    P_Int8 -> numTypeDetails "i8"
    P_Int16 -> numTypeDetails "i16"
    P_Int32 -> numTypeDetails "i32"
    P_Int64 -> numTypeDetails "i64"
    P_Word8 -> numTypeDetails "u8"
    P_Word16 -> numTypeDetails "u16"
    P_Word32 -> numTypeDetails "u32"
    P_Word64 -> numTypeDetails "u64"
    P_Bool -> primTypeDetails "bool" toBool
    P_Void -> primTypeDetails "()" toVoid
    P_ByteVector -> primTypeDetails "Vec<u8>" toByteVector
    P_Json -> primTypeDetails "serde_json::Value" toJson
    P_Vector -> vectorTypeDetails
    P_StringMap -> stringMapTypeDetails
    P_Nullable -> nullableTypeDetails
  where
    primTypeDetails t convf = TypeDetails (const (return t)) convf

    numTypeDetails t = TypeDetails (const (return t)) toNumber
      where
        toNumber (Literal _ (LPrimitive (JS.Number n))) = return (litNumber n <> "_" <> t)
        toNumber _ = error "BUG: expected a numeric literal"

    toString (Literal _ (LPrimitive (JS.String s))) = return (T.pack (show s) <> ".to_string()")
    toString _ = error "BUG: expected a string literal"

    toBool (Literal _ (LPrimitive (JS.Bool True))) = return "true"
    toBool (Literal _ (LPrimitive (JS.Bool False))) = return "false"
    toBool _ = error "BUG: expected a boolean literal"

    toVoid _= return "()"

    toByteVector (Literal _ (LPrimitive (JS.String v))) =  do
      rbase64 <- rustUse (rustScopedName "base64")
      return (template "$1::decode(\"$2\").unwrap()" [rbase64, v])
    toByteVector _ = error "BUG: expected a string literal for ByteVector"

    toJson (Literal _ (LPrimitive jv)) = do
      rserdejson <- rustUse (rustScopedName "serde_json")
      return (template "$1::from_str(\"$2\").unwrap()" [rserdejson, T.replace "\"" "\\\"" (jsonToText jv)])
    toJson _ = error "BUG: expected a json literal for JSson"

    vectorTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return ("Vec<" <> texpr <> ">")
        typeExpr _ = error "BUG: expected a single type param for Vector"
        literalText (Literal te (LVector ls)) = do
          lits <- mapM genLiteralText ls
          return (template "vec![$1]" [T.intercalate ", " lits])
        literalText _ = error "BUG: invalid literal for Vector"

    stringMapTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "std::collections::HashMap<String,$1>" [texpr])
        typeExpr _ = error "BUG: expected a single type param for StringMap"
        literalText (Literal _ (LStringMap m)) = do
          m' <- traverse genLiteralText m
          return (template "[$1].iter().cloned().collect()" [T.intercalate ", " [ template "(\"$1\".to_string(), $2)" [k,v] | (k,v) <- M.toList m']])
        literalText _ = error "BUG: invalid literal for StringMap"

    nullableTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "Option<$1>" [texpr])
        typeExpr _ = error "BUG: expected a single type param for StringMap"
        literalText (Literal _ (LNullable Nothing)) = return "None"
        literalText (Literal _ (LNullable (Just l))) = do
          lit <- genLiteralText l
          return (template "Some($1)" [lit])
        literalText _ = error "BUG: invalid literal for Nullable"

-- a type defined through a regular ADL declaration
getTypeDetails rt@(RT_Named (scopedName,decl@Decl{d_customType=Nothing})) = TypeDetails typeExpr literalText
  where
    typeExpr typeArgs = do
      declRef <- getDeclRef scopedName
      return (declRef <> typeParamsExpr typeArgs)
    literalText (Literal te LDefault) = error "BUG: literal defaults shouldn't be needed"
    literalText (Literal (TypeExpr (RT_Named (_, Decl{d_type=Decl_Struct struct})) tparams0) (LCtor ls)) = do
      tparams <- mapM genTypeExpr tparams0
      lvs <- mapM genLiteralText ls
      return (template "$1$2{$3}" [structName decl, turbofish tparams, T.intercalate ", " [template "$1 : $2" [f_name f,v] | (f,v) <- zip (s_fields struct) lvs]])
    literalText (Literal (TypeExpr (RT_Named (_, Decl{d_type=Decl_Newtype _})) _) (LCtor [l])) = do
      genLiteralText l
    literalText (Literal te@(TypeExpr (RT_Named (sn, Decl{d_type=Decl_Union union})) _) (LUnion ctor l)) = do
      let variantName  =  enumVariantName0 ctor
      declRef <- getDeclRef sn
      lv <- genLiteralText l
      case te of
       te| isVoidLiteral l -> return (template "$1::$2" [declRef, variantName])
          | otherwise -> return (template "$1::$2($3)" [declRef, variantName, lv])
    literalText l = error ("BUG: missing RT_Named literalText definition (" <> show l <> ")")

-- a custom type
getTypeDetails rt@(RT_Named (_,Decl{d_customType=Just customType})) =
  error "BUG: custom types not implemented"

-- a type variable
getTypeDetails (RT_Param typeVar) = TypeDetails typeExpr literalText
  where
    typeExpr _ = return typeVar
    literalText _ = error "BUG: literal values for type variables shouldn't be needed"

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
typeParamsExpr []         = ""
typeParamsExpr parameters = "<" <> T.intercalate ", " parameters <> ">"

turbofish :: [T.Text] -> T.Text
turbofish [] = ""
turbofish tparams = "::" <> typeParamsExpr tparams

traitTypeParamsExpr :: T.Text -> [T.Text] -> T.Text
traitTypeParamsExpr _ []         = T.pack ""
traitTypeParamsExpr trait parameters = "<" <> T.intercalate ", " [template "$1: $2" [p,trait] | p <- parameters] <> ">"

structName :: CDecl -> T.Text
structName decl = capitalise (d_name decl)

structFieldName :: FieldDetails -> T.Text
structFieldName fd = unreserveWord (snakify (f_name (fdField fd)))

enumVariantName :: FieldDetails -> T.Text
enumVariantName fd = enumVariantName0 (f_name (fdField fd))

enumVariantName0 :: T.Text -> T.Text
enumVariantName0 fname = capitalise (camelize fname)


findUnionField :: T.Text -> [Field CResolvedType] -> (Int,Field CResolvedType)
findUnionField fname fs = case L.find (\(_,f) -> f_name f == fname) (zip [0,1..] fs) of
  (Just v) -> v
  Nothing -> error ("BUG: invalid literal " <> show fname <> "for union")

modulePrefix :: [Ident] -> T.Text
modulePrefix [] = T.pack ""
modulePrefix modules = T.intercalate "::" modules <> "::"

generateCode :: Annotations t -> Bool
generateCode annotations = case M.lookup snRustGenerate annotations of
  Just (_,JSON.Bool gen) -> gen
  _ -> True

-- Get the a typescript reference corresponding to an ADL scoped name,
-- (TODO: generate imports to avoid fully scoping every reference)
getDeclRef :: ScopedName -> CState T.Text
getDeclRef sn = do
  currentModuleName <- mfModuleName <$> get
  if sn_moduleName sn == currentModuleName
    then return (sn_name sn)
    else do
      mfn <- mfRustModuleFn <$> get
      rAdlType <- rustUse (RustScopedName (unRustScopedName (mfn (sn_moduleName sn)) <> [sn_name sn]))
      return rAdlType

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType _ _ = Nothing

emptyModuleFile :: ModuleName -> RustFlags -> CodeGenProfile -> ModuleFile
emptyModuleFile mn rf cgp = ModuleFile mn M.empty [] (rustModuleFn rf) cgp

rustModuleFn :: RustFlags -> RustModuleFn
rustModuleFn rf = \mn -> RustScopedName (["crate"] <> unRustScopedName (rsModule rf) <> unModuleName mn)

moduleFilePath  :: [Ident] -> FilePath
moduleFilePath path = joinPath (map T.unpack path)

phantomData :: Ident -> CState T.Text
phantomData typeParam = do
  rPhantomData <- rustUse (rustScopedName "std::marker::PhantomData")
  return (template "$1<$2>" [rPhantomData, typeParam])

rustUse :: RustScopedName -> CState T.Text
rustUse rsname = do
  state <- get
  let userRefs =mfUserRefs state
      shortName = last (unRustScopedName rsname)
      asCandidates = [shortName] <> [shortName <> "_" <> fshow n | n <- [1,2..]]
      uniqueShortName = head (filter (shortNameOk userRefs) asCandidates)
  put state{mfUserRefs=M.insert uniqueShortName rsname userRefs}

  return uniqueShortName
  where
    shortNameOk userRefs n = case M.lookup n userRefs of
      Nothing -> True
      Just rsname1 -> rsname == rsname1

jsonToText :: JS.Value -> T.Text
jsonToText = LT.toStrict . JS.encodeToLazyText

reservedWords :: S.Set Ident
reservedWords = S.fromList
 [ "type"
 -- Fixme: complete this list
 ]

unreserveWord :: Ident -> Ident
unreserveWord n | S.member n reservedWords = T.append "r#" n
                | otherwise = n


snRustGenerate :: ScopedName
snRustGenerate = ScopedName (ModuleName ["adlc","config","rust"]) "RustGenerate"

snRustBoxed :: ScopedName
snRustBoxed = ScopedName (ModuleName ["adlc","config","rust"]) "RustBoxed"
