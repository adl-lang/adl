{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Internal where

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
data TypescriptFlags = TypescriptFlags {
  tsLibDir :: FilePath,
  tsIncludeRuntime :: Bool,
  tsIncludeResolver :: Bool,
  tsExcludeAst :: Bool,
  tsExcludedAstAnnotations :: Maybe [ScopedName],
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

data CodeGenProfile = CodeGenProfile {
  cgp_includeAst :: Bool,
  cgp_includeAstAnnotation :: ScopedName -> Bool
}

-- We use a state monad to accumulate details of the typescript file
-- corresponding to each ADL module
type CState a = State ModuleFile a

data ModuleFile = ModuleFile {
  mfModuleName   :: ModuleName,

  -- The imports upon which this module depends
  mfImports      :: M.Map Ident TSImport,

  -- The code
  mfDeclarations :: [Code],

  -- Details to control the code generate
  mfCodeGenProfile :: CodeGenProfile
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

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mfDeclarations=code:mfDeclarations mf})

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails field = do
  let te = f_type field
  typeExprStr <- genTypeExpr te
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
    P_String -> primTypeDetails "string" toString
    P_Double -> primTypeDetails "number" toNumber
    P_Float -> primTypeDetails "number" toNumber
    P_Int8 -> primTypeDetails "number" toNumber
    P_Int16 -> primTypeDetails "number" toNumber
    P_Int32 -> primTypeDetails "number" toNumber
    P_Int64 -> primTypeDetails "number" toNumber
    P_Word8 -> primTypeDetails "number" toNumber
    P_Word16 -> primTypeDetails "number" toNumber
    P_Word32 -> primTypeDetails "number" toNumber
    P_Word64 -> primTypeDetails "number" toNumber
    P_Bool -> primTypeDetails "boolean" toBool
    P_Void -> primTypeDetails "null" (const (return "null"))
    P_ByteVector -> primTypeDetails "Uint8Array" toByteVector
    P_Json -> primTypeDetails "{}|null" toAny
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

    toByteVector (Literal _ (LPrimitive (JS.String v))) =  return (template "b64.toByteArray(\"$1\")" [v])
    toByteVector _ = error "BUG: expected a string literal for ByteVector"

    toAny (Literal _ (LPrimitive jv)) = return (jsonToText jv)
    toAny _ = error "BUG: expected a json literal for JSson"

    vectorTypeDetails = TypeDetails typeExpr literalText typeValue
      where
        typeExpr [texpr] = return (texpr <> "[]")
        typeExpr _ = error "BUG: expected a single type param for Vector"
        literalText (Literal te (LVector ls)) = do
          lits <- mapM genLiteralText ls
          return (template "[$1]" [T.intercalate ", " lits])
        literalText _ = error "BUG: invalid literal for Vector"
        typeValue [tvalue] = return (template "ADL.texprVector($1)" [tvalue])
        typeValue _ = error "BUG: expected a single type param for Vector"

    stringMapTypeDetails = TypeDetails typeExpr literalText typeValue
      where
        typeExpr [texpr] = return (template "{[key: string]: $1}" [texpr])
        typeExpr _ = error "BUG: expected a single type param for StringMap"
        literalText (Literal _ (LStringMap m)) = do
          m' <- traverse genLiteralText m
          return (template "{$1}" [T.intercalate ", " [ template "$1 : $2" [k,v] | (k,v) <- M.toList m']])
        literalText _ = error "BUG: invalid literal for StringMap"
        typeValue [tvalue] = return (template "ADL.texprStringMap($1)" [tvalue])
        typeValue _ = error "BUG: expected a single type param for StringMap"

    nullableTypeDetails = TypeDetails typeExpr literalText typeValue
      where
        typeExpr [texpr] = return (template "($1|null)" [texpr])
        typeExpr _ = error "BUG: expected a single type param for StringMap"
        literalText (Literal _ (LNullable Nothing)) = return "null"
        literalText (Literal _ (LNullable (Just l))) = genLiteralText l
        literalText _ = error "BUG: invalid literal for Nullable"
        typeValue [tvalue] = return (template "ADL.texprNullable($1)" [tvalue])
        typeValue _ = error "BUG: expected a single type param for Nullable"

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

findUnionField :: T.Text -> [Field CResolvedType] -> (Int,Field CResolvedType)
findUnionField fname fs = case L.find (\(_,f) -> f_name f == fname) (zip [0,1..] fs) of
  (Just v) -> v
  Nothing -> error ("BUG: invalid literal " <> show fname <> "for union")

addImport :: Ident -> TSImport -> CState ()
addImport moduleIdentity tsImport = modify (\mf->mf{mfImports=M.insert moduleIdentity tsImport (mfImports mf)})

addModulesImport :: [Ident] -> T.Text -> CState ()
addModulesImport [] _ = return ()
addModulesImport modules _ = addImport importAsName tsImport
    where
      tsImport = TSImport{iAsName=importAsName, iModulePath=modules}
      importAsName = T.intercalate "_" modules

modulePrefix :: [Ident] -> T.Text
modulePrefix [] = T.pack ""
modulePrefix modules = T.intercalate "_" modules <> "."

renderInterface :: InterfaceName -> ParameterNames -> [FieldDetails] -> Bool -> Code
renderInterface name typeParams fields isPrivate =
  cblock (template "$1interface $2$3" [export, name, typeParamsExpr typeParams]) renderedFields
  where
    export = if isPrivate then "" else "export "
    renderedFields = mconcat [renderCommentForField (fdField fd) <> renderFieldDeclaration fd ";"| fd <- fields]

    renderFieldDeclaration :: FieldDetails -> T.Text -> Code
    renderFieldDeclaration fd endChar
      | fdOptional fd = ctemplate "$1?: $2$3" [fdName fd, fdTypeExprStr fd, endChar]
      | otherwise = ctemplate "$1: $2$3" [fdName fd, fdTypeExprStr fd, endChar]

-- Generate the factory method for a named object
-- Takes in the name, type parameters and field details of object.
renderFactory :: T.Text -> ParameterNames -> [FieldDetails] -> Code
renderFactory name typeParams fds = function
  where
    function
      =  ctemplate "export function make$1$2(" [name, tparams]
      <> indent factoryInputVariable
      <> cline (template "): $1$2 {" [name, tparams])
      <> indent fieldInitialisations
      <> cline "}"
    tparams = typeParamsExpr typeParams
    factoryInputVariable = renderFactoryInput fds
    fieldInitialisations = renderFieldInitialisations fds
    inputVar = case fds of
      [] -> "_input"
      _  -> "input"

    renderFactoryInput fds = cblock (inputVar <> ":") fields
      where
        fields = mconcat [renderInputField fd | fd <- fds]
        renderInputField fd = case fdDefValue fd of
          Nothing -> ctemplate "$1: $2," [fdName fd, fdTypeExprStr fd]
          Just _  -> ctemplate "$1?: $2," [fdName fd, fdTypeExprStr fd]

    renderFieldInitialisations :: [FieldDetails] -> Code
    renderFieldInitialisations fds = cblock1 "return" fieldInitialisations
      where fieldInitialisations = mconcat [renderFieldInitialisation fd | fd <- fds]

    renderFieldInitialisation :: FieldDetails -> Code
    renderFieldInitialisation fd = case fdDefValue fd of
      Nothing -> ctemplate "$1: $2.$1," [fdName fd, inputVar]
      Just defaultValue ->
        cspan (cspan (ctemplate "$1: $2.$1 === undefined ? " [fdName fd,inputVar]) (cline defaultValue)) (ctemplate " : $2.$1," [fdName fd,inputVar])

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

-- generate the type parameters for a declaration, renaming any unused type params to start
-- with an _ (so as to pass lint checkse_
prefixUnusedTypeParams :: (T.Text -> Bool) -> [T.Text] -> [T.Text]
prefixUnusedTypeParams isParamUsed parameters = [if isParamUsed p then p else ("_" <> p) | p <- parameters]

addAstDeclaration :: CModule -> CDecl -> CState ()
addAstDeclaration m decl = do
  cgp <- mfCodeGenProfile <$> get
  when (cgp_includeAst cgp) $ do
    let astDecl
          =  ctemplate "const $1 : ADL.ScopedDecl =" [astVariableName decl]
          <> indent (ctemplate "$1;" [jsonToText (scopedDeclAst cgp m decl)])
        typeExprFn0
          =  cblock (template "export function texpr$1(): ADL.ATypeExpr<$1>" [d_name decl])
                    (ctemplate "return {value : {typeRef : {kind: \"reference\", value : {moduleName : \"$1\",name : \"$2\"}}, parameters : []}};"
                               [formatText (m_name m), d_name decl])
        typeExprFnN tparams
          =  cblock (template "export function texpr$1$2($3): ADL.ATypeExpr<$1$2>"
                              [d_name decl, typeParamsExpr tparams, exprArgs])
                    (ctemplate "return {value : {typeRef : {kind: \"reference\", value : {moduleName : \"$1\",name : \"$2\"}}, parameters : [$3]}};"
                               [formatText (m_name m), d_name decl, exprParams])
             where
               exprArgs = T.intercalate ", " [template "texpr$1 : ADL.ATypeExpr<$1>" [t] | t <- tparams]
               exprParams = T.intercalate ", " [template "texpr$1.value" [t] | t <- tparams]
    addDeclaration astDecl
    case getTypeParams (d_type decl) of
      []      -> addDeclaration typeExprFn0
      tparams -> addDeclaration (typeExprFnN tparams)


astVariableName :: CDecl -> T.Text
astVariableName decl = capitalise (d_name decl) <> "_AST"

addAstMap :: CModule -> CState ()
addAstMap m = do
  addDeclaration $
    cline "export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {"
    <> indent (mconcat [ctemplate "\"$1\" : $2$3" [scopedName m decl, astVariableName decl, mcomma]
                       | (decl,mcomma) <- withCommas includedDecls])
    <> cline "};"
  where
    includedDecls = filter (\decl-> generateCode (d_annotations decl)) (getOrderedDecls m)

scopedName :: CModule -> CDecl -> T.Text
scopedName m d = formatText (ScopedName (m_name m) (d_name d))

scopedDeclAst :: CodeGenProfile -> CModule -> CDecl -> JS.Value
scopedDeclAst cgp m decl = JS.object
  [ ("moduleName", moduleNameAst (m_name m))
  , ("decl", declAst cgp decl)
  ]

declAst :: CodeGenProfile -> CDecl -> JS.Value
declAst cgp decl = JS.object
 [ ("annotations", annotationsAst cgp (d_annotations decl))
 , ("name", JS.toJSON (d_name decl))
 , ("version", mkMaybe Nothing)
 , ("type_", (declTypeAst cgp (d_type decl)))
 ]

declTypeAst :: CodeGenProfile -> DeclType CResolvedType -> JS.Value
declTypeAst cgp (Decl_Struct s) = mkUnion "struct_" (structAst cgp s)
declTypeAst cgp (Decl_Union u) = mkUnion "union_" (unionAst cgp u)
declTypeAst cgp (Decl_Typedef t) = mkUnion "type_"  (typeAst cgp t)
declTypeAst cgp (Decl_Newtype n) = mkUnion "newtype_"  (newtypeAst cgp n)

structAst :: CodeGenProfile -> Struct CResolvedType -> JS.Value
structAst cgp s = JS.object
  [ ("fields", JS.toJSON (map (fieldAst cgp) (s_fields s)))
  , ("typeParams", JS.toJSON (s_typeParams s))
  ]

unionAst :: CodeGenProfile -> Union CResolvedType -> JS.Value
unionAst cgp u = JS.object
  [ ("fields", JS.toJSON (map (fieldAst cgp) (u_fields u)))
  , ("typeParams", JS.toJSON (u_typeParams u))
  ]

typeAst :: CodeGenProfile -> Typedef CResolvedType -> JS.Value
typeAst cgp t = JS.object
  [ ("typeExpr", typeExprAst (t_typeExpr t))
  , ("typeParams", JS.toJSON (t_typeParams t))
  ]

newtypeAst :: CodeGenProfile -> Newtype CResolvedType -> JS.Value
newtypeAst cgp n = JS.object
  [ ("typeExpr", typeExprAst (n_typeExpr n))
  , ("typeParams", JS.toJSON (n_typeParams n))
  , ("default", mkMaybe (n_default n))
  ]

fieldAst :: CodeGenProfile -> Field CResolvedType -> JS.Value
fieldAst cgp f = JS.object
 [ ("name", JS.toJSON (f_name f))
 , ("serializedName", JS.toJSON (f_serializedName f))
 , ("typeExpr", typeExprAst (f_type f))
 , ("default", mkMaybe (f_default f))
 , ("annotations", annotationsAst cgp (f_annotations f))
 ]

annotationsAst :: CodeGenProfile -> Annotations CResolvedType -> JS.Value
annotationsAst cgp as = mapAst scopedNameAst id [(k,v) | (k,(_,v)) <- (M.toList as), cgp_includeAstAnnotation cgp k]

mapAst :: (k -> JS.Value) -> (v -> JS.Value) -> [(k,v)] -> JS.Value
mapAst kf vf kvs = JS.toJSON [ kvAst (kf k) (vf v) | (k,v) <- kvs]
  where
    kvAst kjv vjv = JS.object [ ("v1", kjv), ("v2", vjv) ]

typeExprAst :: TypeExpr CResolvedType -> JS.Value
typeExprAst (TypeExpr tr tes) = JS.object
 [ ("typeRef", typeRefAst tr)
 , ("parameters", JS.toJSON (map typeExprAst tes))
 ]

typeRefAst :: CResolvedType -> JS.Value
typeRefAst (RT_Named (sn,_)) = mkUnion "reference" (scopedNameAst sn)
typeRefAst (RT_Param id) = mkUnion "typeParam" (JS.toJSON id)
typeRefAst (RT_Primitive p) = mkUnion "primitive" (JS.toJSON (ptToText p))

scopedNameAst :: ScopedName -> JS.Value
scopedNameAst (ScopedName moduleName name) = JS.object
  [ ("moduleName", moduleNameAst moduleName)
  , ("name", JS.toJSON name)
  ]

moduleNameAst :: ModuleName -> JS.Value
moduleNameAst (ModuleName mn) = JS.toJSON (T.intercalate "." mn)

literalAst :: JS.Value -> JS.Value
literalAst (JS.Array jvs) = mkUnion "array" (JS.toJSON (map literalAst (V.toList jvs)))
literalAst (JS.Bool b) = mkUnion "boolean" (JS.toJSON b)
literalAst (JS.Number v) | isInteger v = mkUnion "integer" (JS.toJSON v) -- Should git rid of this distinction in adlast.adl
                          | otherwise = mkUnion "double" (JS.toJSON v)
literalAst (JS.Null) = mkVoidUnion "null"
literalAst (JS.Object hm) = mkUnion "object" map
  where
    map = JS.toJSON [JS.object [("v1",JS.toJSON k),("v2",literalAst v)] | (k,v) <- HM.toList hm]
literalAst (JS.String s) = mkUnion "string" (JS.toJSON s)

mkMaybe :: Maybe JS.Value -> JS.Value
mkMaybe Nothing = JS.object [("kind", (JS.toJSON ("nothing"::T.Text)))]
mkMaybe (Just jv) = JS.object
  [ ("kind", (JS.toJSON ("just"::T.Text)))
  , ("value", jv)
  ]

mkUnion :: T.Text -> JS.Value -> JS.Value
mkUnion kind value = JS.object
  [ ("kind", (JS.toJSON kind))
  , ("value", value)
  ]

mkVoidUnion :: T.Text -> JS.Value
mkVoidUnion kind = JS.object
  [ ("kind", (JS.toJSON kind))
  ]

jsonToText :: JS.Value -> T.Text
jsonToText = LT.toStrict . JS.encodeToLazyText

withCommas :: [a] -> [(a,T.Text)]
withCommas [] = []
withCommas [a] = [(a,"")]
withCommas (a:as) = (a,","):withCommas as

-- Check if all fields of the union are void.
isUnionEnum :: Union CResolvedType -> Bool
isUnionEnum u = all (isVoid . f_type) (u_fields u)

isVoid :: CTypeExpr -> Bool
isVoid (TypeExpr (RT_Primitive P_Void) _)  = True
isVoid _ = False

emptyModuleFile :: ModuleName -> CodeGenProfile -> ModuleFile
emptyModuleFile mn cgp = ModuleFile mn M.empty [] cgp

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType _ _ = Nothing

moduleFilePath  :: [Ident] -> FilePath
moduleFilePath path = joinPath (map T.unpack path)

genCode :: Code -> LBS.ByteString
genCode code = LBS.fromStrict (T.encodeUtf8 (T.unlines (codeText Nothing code)))

genModuleCode :: T.Text -> ModuleFile -> LBS.ByteString
genModuleCode cmd mf = genCode code
  where
    code
      =  ctemplate "/* $1generated from adl module $2 */" ["@", formatText (mfModuleName mf)]
      <> cline ""
      <> mconcat [genImport (mfModuleName mf) i | i <- M.elems (mfImports mf)]
      <> cline ""
      <> mconcat (L.intersperse (cline "") (reverse (mfDeclarations mf)))

genImport :: ModuleName -> TSImport -> Code
genImport intoModule TSImport{iAsName=asName, iModulePath=importPath} = ctemplate "import * as $1 from \'$2\';" [asName, mpath]
  where
    mpath = T.intercalate "/" (".":relativeImport)

    intoPath = unModuleName intoModule
    relativeImport = relativePath (init intoPath) (init importPath) ++ [last importPath]

    relativePath [] ps2 = ps2
    relativePath (p1:ps1) (p2:ps2) | p1 == p2 = relativePath ps1 ps2
    relativePath ps1 ps2 = (map (const "..") ps1) <> ps2

generateCode :: Annotations t -> Bool
generateCode annotations = case M.lookup snTypescriptGenerate annotations of
  Just (_,JSON.Bool gen) -> gen
  _ -> True

snTypescriptGenerate :: ScopedName
snTypescriptGenerate = ScopedName (ModuleName ["adlc","config","typescript"]) "TypescriptGenerate"
