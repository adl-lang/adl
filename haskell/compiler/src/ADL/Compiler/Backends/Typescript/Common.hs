{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Common where

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
import ADL.Compiler.Backends.Typescript.DataTypes
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

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mfDeclarations=code:mfDeclarations mf})

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails field = do
  typeExprStr <- genTypeExpr (f_type field)
  let defValueStr = fmap (genLiteralValue (f_type field) M.empty) (f_default field)
  return (FieldDetails field (f_name field) typeExprStr False defValueStr)

-- | Generate the typescript type given an ADL type expression
genTypeExpr :: CTypeExpr -> CState T.Text
genTypeExpr (TypeExpr (RT_Primitive P_Double) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Float) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_String) _) = return "string"
genTypeExpr (TypeExpr (RT_Primitive P_Int8) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Int16) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Int32) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Int64) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Word8) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Word16) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Word32) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Word64) _) = return "number"
genTypeExpr (TypeExpr (RT_Primitive P_Bool) _) = return "boolean"
genTypeExpr (TypeExpr (RT_Primitive P_Void) _) = return "null"
genTypeExpr (TypeExpr (RT_Primitive P_ByteVector) _) = return "Uint8Array"

genTypeExpr (TypeExpr (RT_Primitive P_Vector) [texpr]) = do
  texprStr <- genTypeExpr texpr
  return (texprStr <> "[]")
genTypeExpr (TypeExpr (RT_Primitive P_Vector) _) = error "BUG: Vector must have 1 type parameter"

genTypeExpr (TypeExpr (RT_Primitive P_StringMap) [texpr]) = do
  texprStr <- genTypeExpr texpr
  return (template "{[key: string]: $1}" [texprStr])
genTypeExpr (TypeExpr (RT_Primitive P_StringMap) _) = error "BUG: StringMap must have 1 type parameter"

genTypeExpr (TypeExpr (RT_Primitive P_Nullable) [texpr]) = do
  texprStr <- genTypeExpr texpr
  return (template "($1|null)" [texprStr])
genTypeExpr (TypeExpr (RT_Primitive P_Nullable) _) = error "BUG: Nullable must have 1 type parameter"

genTypeExpr (TypeExpr (RT_Param parameterName) _) = return parameterName

genTypeExpr (TypeExpr (RT_Named (ScopedName moduleName name, _)) parameters) = do
    parametersExpressions <- mapM genTypeExpr parameters
    currentModuleName <- fmap mfModuleName get
    let modules =  if moduleName == currentModuleName then [] else unModuleName moduleName
    addModulesImport modules name
    return (modulePrefix modules <> name <> typeParamsExpr parametersExpressions)

genAnnotation :: ScopedName -> JS.Value -> CState T.Text
genAnnotation _ _ = error "FIXME: genAnnotation not implemented"

addImport :: Ident -> TSImport -> CState ()
addImport moduleIdentity tsImport = modify (\mf->mf{mfImports=M.insert moduleIdentity tsImport (mfImports mf)})

addModulesImport :: [Ident] -> T.Text -> CState ()
addModulesImport [] _ = return ()
addModulesImport modules _ = addImport importAsName tsImport
    where
      tsImport = TSImport{iAsName=importAsName, iModulePath=modules}
      importAsName = T.intercalate "_" modules

type BoundTypeVariables = M.Map Ident CTypeExpr

genLiteralValue :: CTypeExpr -> BoundTypeVariables -> JS.Value -> T.Text
genLiteralValue (TypeExpr (RT_Primitive p) tparams) btv jv = case p of
  P_Double -> toNumber jv
  P_Float -> toNumber jv
  P_Int8 -> toNumber jv
  P_Int16 -> toNumber jv
  P_Int32 -> toNumber jv
  P_Int64 -> toNumber jv
  P_Word8 -> toNumber jv
  P_Word16 -> toNumber jv
  P_Word32 -> toNumber jv
  P_Word64 -> toNumber jv
  P_String -> case jv of
    JS.String v -> template "'$1'" [v]
    _ -> error "BUG: expected a string literal"
  P_Bool -> case jv of
    JS.Bool True -> "true"
    JS.Bool False -> "false"
    _ -> error "BUG: expected a boolean literal"
  P_Void -> "null"
  P_Vector -> case tparams of
    [texpr] -> case jv of
      JS.Array values -> template "[$1]" [T.intercalate ", " (map (genLiteralValue texpr btv) (V.toList values))]
      _ -> error "BUG: expected an array literal for Vector"
    _ -> error "BUG: expected a single type parameter for Vector"
  P_ByteVector -> case jv of
    JS.String v -> template "b64.toByteArray('$1')" [v]
    _ -> error "BUG: expected a string literal for bytevector"
  P_StringMap -> case tparams of
    [texpr] -> case jv of
      JS.Object hm -> template "{$1}"
        [T.intercalate ", "
         [template "$1 : $2" [k, genLiteralValue texpr btv v] | (k,v) <- HM.toList hm]
        ]
      _ -> error "BUG: expected an object literal for StringMap"
  P_Nullable -> case tparams of
    [texpr] -> case jv of
      JS.Null -> "null"
      _ -> genLiteralValue texpr btv jv
    _ -> error "BUG: expected a single type parameter for Nullable"
  where
    toNumber (JS.Number n) = litNumber n
    toNumber _ = error "BUG: expected a number literal"

genLiteralValue (TypeExpr (RT_Named (_, Decl{d_type=Decl_Struct struct})) tparams) btv jv =
  case jv of
    JS.Object hm -> template "{$1}" [T.intercalate ", " (map (renderField hm) (s_fields struct))]
    _ -> error "BUG: expected an object literal for struct"
  where
    btv' = createBoundTypeVariables btv (s_typeParams struct)  tparams
    renderField hm f = template "$1 : $2" [f_name f, genLiteralValue (f_type f) btv' value]
      where
        value = case HM.lookup (f_name f) hm of
          Nothing -> case f_default f of
            Nothing -> error ("BUG: missing default value for " <> T.unpack (f_name f))
            (Just value) -> value
          (Just value) -> value

genLiteralValue (TypeExpr (RT_Named (scopedName, Decl{d_type=Decl_Union union})) tparams) btv value =
  case value of
    JS.String v ->
      case findUnionField v (u_fields union) of
        (i,f) | isUnionEnum union -> T.pack (show i)
              | otherwise -> template "{kind : \"$1\"}" [f_name f]
    JS.Object hm -> case HM.toList hm of
      [(k,value)] ->
        case findUnionField k (u_fields union) of
          (i,f) -> template "{kind : \"$1\", value : $2}" [f_name f, genLiteralValue (f_type f) btv' value]
      _ -> error "BUG: union literal should be a single element object"
    _ -> error "BUG: expected a string or object literal for union"
  where
    btv' = createBoundTypeVariables btv (u_typeParams union)  tparams

genLiteralValue (TypeExpr (RT_Named (_, Decl{d_type=Decl_Typedef typedef})) tparams) btv value =
  genLiteralValue (t_typeExpr typedef) btv' value
  where
    btv' = createBoundTypeVariables btv (t_typeParams typedef)  tparams

genLiteralValue (TypeExpr (RT_Named (_, Decl{d_type=Decl_Newtype ntype})) tparams) btv value =
  genLiteralValue (n_typeExpr ntype) btv' value
  where
    btv' = createBoundTypeVariables btv (n_typeParams ntype)  tparams

genLiteralValue (TypeExpr (RT_Param param) _) btv value =
  case M.lookup param btv of
    Nothing -> error ("BUG: unable to resolve type variable " <> T.unpack param)
    Just texpr -> genLiteralValue texpr btv value

createBoundTypeVariables :: BoundTypeVariables -> [Ident] -> [CTypeExpr] -> BoundTypeVariables
createBoundTypeVariables btv names types = M.union btv (M.fromList (zip names types))

findUnionField :: T.Text -> [Field CResolvedType] -> (Int,Field CResolvedType)
findUnionField fname fs = case L.find (\(_,f) -> f_name f == fname) (zip [0,1..] fs) of
  (Just v) -> v
  Nothing -> error ("BUG: invalid literal " <> show fname <> "for union")

modulePrefix :: [Ident] -> T.Text
modulePrefix [] = T.pack ""
modulePrefix modules = T.intercalate "_" modules <> "."

renderInterface :: InterfaceName -> ParameterNames -> [FieldDetails] -> Bool -> Code
renderInterface name typeParams fields isPrivate =
  cblock (template "$1interface $2$3" [export, name, typeParamsExpr typeParams]) renderedFields
  where
    export = if isPrivate then "" else "export "
    renderedFields = mconcat [renderFieldDeclaration fd ";"| fd <- sortedFds]
    sortedFds = L.sort fields

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
    sortedFds = L.sort fds
    tparams = typeParamsExpr typeParams
    factoryInputVariable = renderFactoryInput sortedFds
    fieldInitialisations = renderFieldInitialisations sortedFds

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
renderCommentsForDeclaration decl = renderComments $ M.elems (d_annotations decl)
  where
    renderComments :: [(CResolvedType, JS.Value)] -> Code
    renderComments = L.foldr (CAppend . renderComment) CEmpty

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
    cline "export const _AST_MAP = {"
    <> indent (mconcat [ctemplate "\"$1\" : $2$3" [scopedName m decl, astVariableName decl, mcomma]
                       | (decl,mcomma) <- withCommas (M.elems (m_decls m))])
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
