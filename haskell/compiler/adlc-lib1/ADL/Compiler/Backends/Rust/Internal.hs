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
import qualified ADL.Adlc.Config.Rust as RA

import ADL.Core
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
  rs_libDir :: FilePath,

  -- The rust module into which we generate the ADL
  -- relative to the crate root
  rs_module :: RustScopedName,

  -- The absolute rust module path containing the runtime
  -- relative to the crate root
  rs_runtimeModule :: RustScopedName,

  -- Whether to output the runtime code
  rs_includeRuntime :: Bool
}

data RustScopedName = RustScopedName {unRustScopedName :: [Ident]}
  deriving (Eq, Ord, Show)

type RustModuleFn = ModuleName -> RustScopedName

rustScopedName :: T.Text -> RustScopedName
rustScopedName s = RustScopedName (T.splitOn "::" s)

data CodeGenProfile = CodeGenProfile {
}


-- Custom Type definitions where we want to substitute a
-- user defined type in lieu of an ADL generated one
data CustomType = CustomType {
  ct_scopedName :: RustScopedName,
  ct_helpers :: RustScopedName,
  ct_generateOrigType :: Maybe Ident,
  ct_stdTraits :: StdTraits
} deriving (Show)

-- We use a state monad to accumulate details of the rust file
-- 
-- corresponding to each ADL module
type CState a = State ModuleFile a

data ModuleFile = ModuleFile {
  mf_moduleName   :: ModuleName,

  -- The use references for this module
  mf_useRefs     :: M.Map Ident RustScopedName,

  -- The code
  mf_declarations :: [Code],

  -- Function mappying ADL modules to rust modules
  mf_rustModuleFn :: RustModuleFn,

  -- Details to control the code generate
  mf_codeGenProfile :: CodeGenProfile
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
  fd_field       :: Field CResolvedType,
  fd_typeExprStr :: T.Text,
  fd_defValue    :: Maybe T.Text
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
      =  ctemplate "// $1generated from adl module $2" ["@", formatText (mf_moduleName mf)]
      <> (if M.null (mf_useRefs mf) then mempty else cline "")
      <> mconcat [genUse shortName rsname | (shortName,rsname) <- (L.sortOn snd (M.toList (mf_useRefs mf)))]
      <> cline ""
      <> mconcat (L.intersperse (cline "") (reverse (mf_declarations mf)))

    genCode code = LBS.fromStrict (T.encodeUtf8 (T.unlines (codeText Nothing code)))

    genUse shortName rsname =
      if shortName == last (unRustScopedName rsname)
        then ctemplate "use $1;" [scopedName]
        else ctemplate "use $1 as $2;" [scopedName, shortName]
      where
        scopedName = T.intercalate "::" (unRustScopedName rsname)

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mf_declarations=code:mf_declarations mf})

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails field = do
  let te = f_type field
      storageModel = getRustStorageModel field
  typeExprStr0 <- genTypeExpr te
  let typeExprStr = storageTypeExpr storageModel typeExprStr0
  defValueStr <- case f_default field of
    (Just v) -> case literalForTypeExpr te v of
      Left e -> error ("BUG: invalid json literal: " ++ T.unpack e)
      Right litv -> do
        valueStr <- genLiteralText litv
        return (Just valueStr)
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
    P_TypeToken -> typeTokenTypeDetails
  where
    primTypeDetails t convf = TypeDetails (const (return t)) convf

    numTypeDetails t = TypeDetails (const (return t)) toNumber
      where
        toNumber (Literal _ (LPrimitive (JS.Number n))) = return (litNumber n <> "_" <> t)
        toNumber _ = error "BUG: expected a numeric literal"

    toString (Literal _ (LPrimitive (JS.String s))) = return (doubleQuote s <> ".to_string()")
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
          return (template "[$1].iter().cloned().collect()" [T.intercalate ", " [ template "($1.to_string(), $2)" [doubleQuote k,v] | (k,v) <- M.toList m']])
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

    typeTokenTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "std::marker::PhantomData<$1>" [texpr])
        typeExpr _ = error "BUG: expected a single type param for TypeToken"
        literalText _ = return "std::marker::PhantomData"

-- a type defined through a regular ADL declaration
getTypeDetails rt@(RT_Named (sn,decl@Decl{d_customType=Nothing})) = TypeDetails typeExpr literalText
  where
    typeExpr typeArgs = do
      declRef <- getDeclRef (sn_moduleName sn) (sn_name sn)
      return (declRef <> typeParamsExpr typeArgs)
    literalText (Literal te LDefault) = error "BUG: literal defaults shouldn't be needed"
    literalText (Literal (TypeExpr (RT_Named (sn, Decl{d_type=Decl_Struct struct})) tparams0) (LCtor ls)) = do
      sname <- getDeclRef (sn_moduleName sn) (structName decl)
      tparams <- mapM genTypeExpr tparams0
      lvs <- mapM genLiteralText ls
      return (template "$1$2{$3}"
         [ sname
         , turbofish tparams
         , T.intercalate ", " [template "$1 : $2" [structFieldName0 (f_name f),v] | (f,v) <- zip (s_fields struct) lvs]
         ])
    literalText (Literal (TypeExpr (RT_Named (sn, Decl{d_type=Decl_Newtype _})) _) (LCtor [l])) = do
      declRef <- getDeclRef (sn_moduleName sn) (sn_name sn)
      lv <- genLiteralText l
      return (template "$1($2)" [declRef,lv])
    literalText (Literal te@(TypeExpr (RT_Named (sn, Decl{d_type=Decl_Union union})) _) (LUnion ctor l)) = do
      let variantName  =  enumVariantName0 ctor
      declRef <- getDeclRef (sn_moduleName sn) (sn_name sn)
      lv <- genLiteralText l
      case te of
       te| isVoidLiteral l -> return (template "$1::$2" [declRef, variantName])
          | otherwise -> return (template "$1::$2($3)" [declRef, variantName, lv])
    literalText l = error ("BUG: missing RT_Named literalText definition (" <> show l <> ")")

-- a custom type
getTypeDetails rt@(RT_Named (_,Decl{d_customType=Just customType})) = TypeDetails typeExpr literalText
  where
    typeExpr typeArgs = do
      rtype <- rustUse (ct_scopedName customType)
      return (rtype <> typeParamsExpr typeArgs)

    literalText (Literal te LDefault) = do
      rHelpers <- rustUse (ct_helpers customType)
      return (template "$1::default()" [rHelpers])
    literalText (Literal te (LCtor ls)) = do
      rHelpers <- rustUse (ct_helpers customType)
      lits <- mapM genLiteralText ls
      return (template "$1::new($2)" [rHelpers, T.intercalate ", " lits])
    literalText (Literal te (LUnion ctor l)) = do
      rHelpers <- rustUse (ct_helpers customType)
      lit <- genLiteralText l
      return (template "$1::$2($3)" [rHelpers, camelize ctor, if isVoidLiteral l then "" else lit ])
    literalText lit = error ("BUG: getTypeDetails2: unexpected literal:" ++ show lit)

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
structFieldName fd = structFieldName0 (f_name (fd_field fd))

structFieldName0 :: T.Text -> T.Text
structFieldName0 fname = unreserveWord (snakify fname)

enumVariantName :: FieldDetails -> T.Text
enumVariantName fd = enumVariantName0 (f_name (fd_field fd))

enumVariantName0 :: T.Text -> T.Text
enumVariantName0 fname = capitalise (camelize fname)


findUnionField :: T.Text -> [Field CResolvedType] -> (Int,Field CResolvedType)
findUnionField fname fs = case L.find (\(_,f) -> f_name f == fname) (zip [0,1..] fs) of
  (Just v) -> v
  Nothing -> error ("BUG: invalid literal " <> show fname <> "for union")

modulePrefix :: [Ident] -> T.Text
modulePrefix [] = T.pack ""
modulePrefix modules = T.intercalate "::" modules <> "::"

-- Determine whether to generate the given CDecl, and if so, whether
-- to rename it for use in a custom type
generateDecl :: CDecl -> Maybe CDecl
generateDecl decl@(Decl{d_customType=mct}) =
  case generateCode (d_annotations decl) of
    False -> Nothing
    True -> case mct of
      Nothing -> Just decl
      Just ct -> case ct_generateOrigType ct of
        Nothing -> Nothing
        (Just asName) -> Just decl{d_name=asName}

generateCode :: Annotations t -> Bool
generateCode annotations = fromMaybe True (getTypedAnnotation snRustGenerate annotations)

-- Get the a typescript reference corresponding to an ADL scoped name,
getDeclRef :: ModuleName -> Ident -> CState T.Text
getDeclRef mn ident = do
  currentModuleName <- mf_moduleName <$> get
  if mn == currentModuleName
    then return ident
    else do
      mfn <- mf_rustModuleFn <$> get
      rAdlType <- rustUse (RustScopedName (unRustScopedName (mfn mn) <> [ident]))
      return rAdlType

-- Get the details of the custom type mapping for a declaration, if any.
getCustomType :: RustScopedName -> ScopedName -> RDecl -> Maybe CustomType
getCustomType runtimeModule sn decl = case getTypedAnnotation rustCustomType (d_annotations decl) of
  Nothing -> Nothing
  (Just rct) -> Just CustomType {
    ct_scopedName = fixRuntimeModule (rustScopedName (RA.rustCustomType_rustname rct)),
    ct_helpers = fixRuntimeModule (rustScopedName (RA.rustCustomType_helpers rct)),
    ct_generateOrigType =
      if T.length (RA.rustCustomType_generateOrigADLType rct) == 0
        then Nothing
        else (Just (RA.rustCustomType_generateOrigADLType rct)),
    ct_stdTraits = S.fromList (RA.rustCustomType_stdTraits rct)
  }
  where
    rustCustomType = ScopedName (ModuleName ["adlc", "config", "rust"]) "RustCustomType"

    fixRuntimeModule :: RustScopedName -> RustScopedName
    fixRuntimeModule rsn = case rsn  of
      RustScopedName (m:ms) | m == "{{STDLIBMODULE}}" -> RustScopedName ("crate" : unRustScopedName runtimeModule <> ms)
      _ -> rsn
   
emptyModuleFile :: ModuleName -> RustFlags -> CodeGenProfile -> ModuleFile
emptyModuleFile mn rf cgp = ModuleFile mn M.empty [] (rustModuleFn rf) cgp

rustModuleFn :: RustFlags -> RustModuleFn
rustModuleFn rf = \mn -> RustScopedName (["crate"] <> unRustScopedName (rs_module rf) <> unModuleName mn)

moduleFilePath  :: [Ident] -> FilePath
moduleFilePath path = joinPath (map T.unpack path)

phantomData :: Ident -> CState T.Text
phantomData typeParam = do
  rPhantomData <- rustUse (rustScopedName "std::marker::PhantomData")
  return (template "$1<$2>" [rPhantomData, typeParam])

rustUse :: RustScopedName -> CState T.Text
rustUse rsname = do
  state <- get
  let userRefs =mf_useRefs state
      shortName = last (unRustScopedName rsname)
      asCandidates = [shortName] <> [shortName <> "_" <> fshow n | n <- [1,2..]]
      uniqueShortName = head (filter (shortNameOk userRefs) asCandidates)
  put state{mf_useRefs=M.insert uniqueShortName rsname userRefs}

  return uniqueShortName
  where
    shortNameOk userRefs n = case M.lookup n userRefs of
      Nothing -> True
      Just rsname1 -> rsname == rsname1

jsonToText :: JS.Value -> T.Text
jsonToText = LT.toStrict . JS.encodeToLazyText

-- | Determine which standard traits exist for
-- a given type expression
--
--
--
type StdTraits = S.Set Ident

stdTraitsFor :: TypeExpr CResolvedType -> StdTraits
stdTraitsFor = stdTraitsFor1 S.empty M.empty

stdTraitsFor1 :: S.Set ScopedName -> TypeBindingMap -> TypeExpr CResolvedType -> StdTraits
stdTraitsFor1 sns tbmap (TypeExpr (RT_Primitive P_Vector) [te]) = stdTraitsFor1 sns tbmap te
stdTraitsFor1 sns tbmap (TypeExpr (RT_Primitive P_StringMap) [te]) = S.delete "Hash" (stdTraitsFor1 sns tbmap te)
stdTraitsFor1 sns tbmap (TypeExpr (RT_Primitive P_Nullable) [te]) = stdTraitsFor1 sns tbmap te
stdTraitsFor1 sns tbmap (TypeExpr (RT_Primitive P_TypeToken) [te]) = stdTraitsFor1 sns tbmap te
stdTraitsFor1 sns tbmap (TypeExpr (RT_Primitive P_Float) _) = noeqStdTraits
stdTraitsFor1 sns tbmap (TypeExpr (RT_Primitive P_Double) _) = noeqStdTraits
stdTraitsFor1 sns tbmap (TypeExpr (RT_Primitive P_Json) _) = noeqStdTraits
stdTraitsFor1 sns tbmap (TypeExpr (RT_Primitive _) _) = defaultStdTraits
stdTraitsFor1 sns tbmap (TypeExpr (RT_Param tp) _) = case M.lookup tp tbmap of
   Nothing -> defaultStdTraits
   (Just te) -> stdTraitsFor1 sns (M.delete tp tbmap) te
stdTraitsFor1 sns tbmap (TypeExpr (RT_Named (sn,decl)) tes) = case S.member sn sns of
  True -> defaultStdTraits
  False ->
    let sns' = S.insert sn sns in
    case decl of
      Decl{d_customType=(Just ct)} -> foldr S.intersection (ct_stdTraits ct) [stdTraitsFor te | te <-tes]
      Decl{d_type=Decl_Struct s} -> stdTraitsForFields1 sns' (withTypeBindings (s_typeParams s) tes tbmap) (s_fields s)
      Decl{d_type=Decl_Union u} -> stdTraitsForFields1 sns' (withTypeBindings (u_typeParams u) tes tbmap) (u_fields u)
      Decl{d_type=Decl_Typedef t} -> stdTraitsFor1 sns' (withTypeBindings (t_typeParams t) tes tbmap) (t_typeExpr t)
      Decl{d_type=Decl_Newtype n} -> stdTraitsFor1 sns' (withTypeBindings (n_typeParams n) tes tbmap) (n_typeExpr n)

stdTraitsForFields1 :: S.Set ScopedName -> TypeBindingMap -> [Field CResolvedType] -> StdTraits
stdTraitsForFields1 sns tbmap fields = foldr S.intersection defaultStdTraits [stdTraitsFor1 sns tbmap (f_type f) | f <- fields]

stdTraitsForFields :: [Field CResolvedType] -> StdTraits
stdTraitsForFields = stdTraitsForFields1 S.empty M.empty

defaultStdTraits = S.fromList ["PartialEq", "Eq", "Hash", "Serialize", "Deserialize", "Clone", "Debug"]
noeqStdTraits = S.fromList ["PartialEq", "Serialize", "Deserialize", "Clone", "Debug"]

type TypeBindingMap = M.Map Ident (TypeExpr CResolvedType)

withTypeBindings :: [Ident] -> [TypeExpr CResolvedType] -> TypeBindingMap -> TypeBindingMap
withTypeBindings ids tes m = M.union m (M.fromList (zip ids tes))

reservedWords :: S.Set Ident
reservedWords = S.fromList
  [ "abstract"
  , "as"
  , "async"
  , "await"
  , "become"
  , "box"
  , "break"
  , "const"
  , "continue"
  , "crate"
  , "do"
  , "dyn"
  , "else"
  , "enum"
  , "extern"
  , "false"
  , "final"
  , "fn"
  , "for"
  , "if"
  , "impl"
  , "in"
  , "let"
  , "loop"
  , "macro"
  , "match"
  , "mod"
  , "move"
  , "mut"
  , "override"
  , "priv"
  , "pub"
  , "ref"
  , "return"
  , "self"
  , "Self"
  , "'static"
  , "static"
  , "struct"
  , "super"
  , "trait"
  , "true"
  , "try"
  , "type"
  , "typeof"
  , "union"
  , "unsafe"
  , "unsized"
  , "use"
  , "virtual"
  , "where"
  , "while"
  , "yield"
  ]

unreserveWord :: Ident -> Ident
unreserveWord n | S.member n reservedWords = T.append "r#" n
                | otherwise = n

storageTypeExpr :: RA.RustStorageModel -> T.Text -> T.Text
storageTypeExpr RA.RustStorageModel_standard texprStr = texprStr
storageTypeExpr RA.RustStorageModel_boxed texprStr = template "Box<$1>" [texprStr]

storageLitValue :: RA.RustStorageModel -> T.Text -> T.Text
storageLitValue RA.RustStorageModel_standard litValueStr = litValueStr
storageLitValue RA.RustStorageModel_boxed litValueStr = template "Box::new($1)" [litValueStr]

getRustStorageModel :: Field CResolvedType -> RA.RustStorageModel
getRustStorageModel f = fromMaybe RA.RustStorageModel_standard (getTypedAnnotation rustStorageModel (f_annotations f))
  where
    rustStorageModel = ScopedName (ModuleName ["adlc","config","rust"]) "RustStorageModel"

getTypedAnnotation :: (AdlValue a) => ScopedName -> Annotations r -> Maybe a
getTypedAnnotation sn annotations = case M.lookup sn annotations of
  Nothing -> Nothing
  (Just (_,jv)) -> case adlFromJson jv of
      (ParseFailure e ctx) -> error (T.unpack (  "BUG: failed to parse annotation: " <> e
                                    <> ", at " <> textFromParseContext ctx))
      (ParseSuccess a) -> Just a

snRustGenerate :: ScopedName
snRustGenerate = ScopedName (ModuleName ["adlc","config","rust"]) "RustGenerate"
