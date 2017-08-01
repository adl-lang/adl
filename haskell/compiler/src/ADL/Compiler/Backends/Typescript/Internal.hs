{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Internal where

import qualified Data.Aeson as JS
import qualified Data.Aeson.Text as JS
import qualified Data.Char as C
import qualified Data.Foldable as F
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V

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

-- | Command line flags to control the backend.
-- (once we have them)
data TypescriptFlags = TypescriptFlags {
  tsLibDir :: FilePath,
  tsIncludeRuntime :: Bool,
  tsExcludeAst :: Bool,
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
  cgp_includeAst :: Bool
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
  -- | Generate the json representation of the type,
  -- given the representation of the type arguments.
  td_type :: [T.Text] -> CState T.Text,

  -- | Generate a typescript literal value
  td_genLiteralText :: Literal CTypeExpr -> CState T.Text

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
    primTypeDetails t convf = TypeDetails (const (return t)) convf

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

    vectorTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (texpr <> "[]")
        typeExpr _ = error "BUG: expected a single type param for Vector"
        literalText (Literal te (LVector ls)) = do
          lits <- mapM genLiteralText ls
          return (template "[$1]" [T.intercalate ", " lits])
        literalText _ = error "BUG: invalid literal for Vector"

    stringMapTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "{[key: string]: $1}" [texpr])
        typeExpr _ = error "BUG: expected a single type param for StringMap"
        literalText (Literal _ (LStringMap m)) = do
          m' <- traverse genLiteralText m
          return (template "{$1}" [T.intercalate ", " [ template "$1 : $2" [k,v] | (k,v) <- M.toList m']])
        literalText _ = error "BUG: invalid literal for StringMap"

    nullableTypeDetails = TypeDetails typeExpr literalText
      where
        typeExpr [texpr] = return (template "($1|null)" [texpr])
        typeExpr _ = error "BUG: expected a single type param for StringMap"
        literalText (Literal _ (LNullable Nothing)) = return "null"
        literalText (Literal _ (LNullable (Just l))) = genLiteralText l
        literalText _ = error "BUG: invalid literal for Nullable"

-- a type defined through a regular ADL declaration
getTypeDetails rt@(RT_Named (scopedName,Decl{d_customType=Nothing})) = TypeDetails typeExpr literalText
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

-- a custom type
getTypeDetails rt@(RT_Named (_,Decl{d_customType=Just customType})) =
  error "BUG: custom types not implemented"

-- a type variable
getTypeDetails (RT_Param typeVar) = TypeDetails typeExpr literalText
  where
    typeExpr _ = return typeVar
    literalText _ = error "BUG: literal values for type variables shouldn't be needed"

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
    renderedFields = mconcat [renderFieldDeclaration fd ";"| fd <- fields]

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

    renderFactoryInput fds = cblock "input:" fields
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
      Nothing -> ctemplate "$1: input.$1," [fdName fd]
      Just defaultValue ->
        cspan (cspan (ctemplate "$1: input.$1 === undefined ? " [fdName fd]) (cline defaultValue)) (ctemplate " : input.$1," [fdName fd])

renderCommentsForDeclaration :: CDecl -> Code
renderCommentsForDeclaration decl = mconcat $ map renderComment $ M.elems (d_annotations decl)
  where
    renderComment :: (CResolvedType, JS.Value) -> Code
    renderComment (RT_Named (ScopedName{sn_name="Doc"}, _), JS.String commentValue) = clineN commentLinesStarred
      where
        commentLinesStarred = ["/**"] ++ [" * " <> commentLine | commentLine <- commentLinesBroken] ++ [" */"]
        commentLinesBroken = L.filter (/= "") (T.splitOn "\n" commentValue)
    renderComment _ = CEmpty

typeParamsExpr :: [T.Text] -> T.Text
typeParamsExpr []         = T.pack ""
typeParamsExpr parameters = "<" <> T.intercalate ", " parameters <> ">"

capitalise :: T.Text -> T.Text
capitalise text = T.cons (C.toUpper (T.head text)) (T.tail text)

addAstDeclaration :: CModule -> CDecl -> CState ()
addAstDeclaration m decl = do
  includeAst <- fmap (cgp_includeAst . mfCodeGenProfile) get
  when includeAst $ do
    let astDecl
          =  ctemplate "const $1 : ADL.ScopedDecl =" [astVariableName decl]
          <> indent (ctemplate "$1;" [jsonToText (scopedDeclAst m decl)])
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
                       | (decl,mcomma) <- withCommas (getOrderedDecls m)])
    <> cline "};"
  where

scopedName :: CModule -> CDecl -> T.Text
scopedName m d = formatText (ScopedName (m_name m) (d_name d))

scopedDeclAst :: CModule -> CDecl -> JS.Value
scopedDeclAst m decl = JS.object
  [ ("moduleName", moduleNameAst (m_name m))
  , ("decl", declAst decl)
  ]

declAst :: CDecl -> JS.Value
declAst decl = JS.object
 [ ("annotations", annotationsAst (d_annotations decl))
 , ("name", JS.toJSON (d_name decl))
 , ("version", mkMaybe Nothing)
 , ("type_", (declTypeAst (d_type decl)))
 ]

declTypeAst :: DeclType CResolvedType -> JS.Value
declTypeAst (Decl_Struct s) = mkUnion "struct_" (structAst s)
declTypeAst (Decl_Union u) = mkUnion "union_" (unionAst u)
declTypeAst (Decl_Typedef t) = mkUnion "type_"  (typeAst t)
declTypeAst (Decl_Newtype n) = mkUnion "newtype_"  (newtypeAst n)

structAst :: Struct CResolvedType -> JS.Value
structAst s = JS.object
  [ ("fields", JS.toJSON (map fieldAst (s_fields s)))
  , ("typeParams", JS.toJSON (s_typeParams s))
  ]

unionAst :: Union CResolvedType -> JS.Value
unionAst u = JS.object
  [ ("fields", JS.toJSON (map fieldAst (u_fields u)))
  , ("typeParams", JS.toJSON (u_typeParams u))
  ]

typeAst :: Typedef CResolvedType -> JS.Value
typeAst t = JS.object
  [ ("typeExpr", typeExprAst (t_typeExpr t))
  , ("typeParams", JS.toJSON (t_typeParams t))
  ]

newtypeAst :: Newtype CResolvedType -> JS.Value
newtypeAst n = JS.object
  [ ("typeExpr", typeExprAst (n_typeExpr n))
  , ("typeParams", JS.toJSON (n_typeParams n))
  , ("default", mkMaybe (fmap literalAst (n_default n)))
  ]

fieldAst :: Field CResolvedType -> JS.Value
fieldAst f = JS.object
 [ ("name", JS.toJSON (f_name f))
 , ("serializedName", JS.toJSON (f_serializedName f))
 , ("typeExpr", typeExprAst (f_type f))
 , ("default", mkMaybe (fmap literalAst (f_default f)))
 , ("annotations", annotationsAst (f_annotations f))
 ]

annotationsAst :: Annotations CResolvedType -> JS.Value
annotationsAst a = JS.toJSON ([]::[()]) -- FIXME and implement

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
