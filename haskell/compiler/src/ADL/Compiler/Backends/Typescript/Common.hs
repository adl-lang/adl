{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Common where

import           ADL.Compiler.AST
import           ADL.Compiler.Backends.Typescript.DataTypes
import           ADL.Compiler.Primitive
import           ADL.Compiler.Processing
import           ADL.Utils.Format                           (template,formatText)
import           ADL.Utils.IndentedCode
import           Control.Monad(when)
import           Control.Monad.Trans.State.Strict
import           Data.Scientific(isInteger)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Text as JS
import qualified Data.Char                                  as C
import qualified Data.Foldable                              as F
import qualified Data.HashMap.Lazy                          as HM
import qualified Data.List                                  as L
import qualified Data.Map                                   as Map
import           Data.Monoid
import qualified Data.Text                                  as T
import qualified Data.Text.Lazy                             as LT
import qualified Data.Vector                                as V


-- * Stateful functions

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mfDeclarations=code:mfDeclarations mf})

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails field = do
  typeExprStr <- genTypeExpr (f_type field)
  defValueStr <- traverse (genLiteralValue (f_type field)) (f_default field)
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

genTypeExpr (TypeExpr (RT_Primitive P_Vector) [vectorEntryTypeExpr]) = do
  vectorEntryOutput <- genTypeExpr vectorEntryTypeExpr
  return (vectorEntryOutput <> "[]")
genTypeExpr (TypeExpr (RT_Primitive P_Vector) _) = error "BUG: Vector must have 1 type parameter"

genTypeExpr (TypeExpr (RT_Param parameterName) _) = return parameterName

genTypeExpr (TypeExpr (RT_Named (ScopedName{sn_name="Nullable"}, _)) nullableTypeExpr) = do
    nonNullableTypeExpr <- genTypeExpr $ L.head nullableTypeExpr
    return (nonNullableTypeExpr <> "|null")

genTypeExpr (TypeExpr (RT_Named (ScopedName moduleName name, _)) parameters) = do
    parametersExpressions <- mapM genTypeExpr parameters
    currentModuleName <- fmap mfModuleName get
    let modules =  if moduleName == currentModuleName then [] else unModuleName moduleName
    addModulesImport modules name
    return $ (renderModulePrefix modules <> name) <> renderParametersExpr parametersExpressions

genTypeExpr _ = return "TYPE??"  -- FIXME: implement

-- | Generate the typescript literal value for the given ADL type and
-- literal
genLiteralValue :: CTypeExpr -> JS.Value -> CState Code
genLiteralValue typeExpr value = return $ renderLiteralValue typeExpr value

genAnnotation :: ScopedName -> JS.Value -> CState T.Text
genAnnotation _ _ = return "UNHANDLED ANNOTATION???"

addImport :: Ident -> TSImport -> CState ()
addImport moduleIdentity tsImport = modify (\mf->mf{mfImports=Map.insert moduleIdentity tsImport (mfImports mf)})

addModulesImport :: [Ident] -> T.Text -> CState ()
addModulesImport [] _ = return ()
addModulesImport modules _ = addImport importAsName tsImport
    where
      tsImport = TSImport{iAsName=importAsName, iModulePath=modules}
      importAsName = T.intercalate "_" modules

-- * Pure functions

renderLiteralValue :: CTypeExpr -> JS.Value -> Code
renderLiteralValue (TypeExpr (RT_Primitive P_Double) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Float) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_String) _) (JS.String value) = ctemplate "'$1'" [value]
renderLiteralValue (TypeExpr (RT_Primitive P_Int8) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Int16) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Int32) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Int64) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Word8) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Word16) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Word32) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Word64) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Bool) _) (JS.Bool value)
  | value = cline "true"
  | otherwise = cline "false"
renderLiteralValue (TypeExpr (RT_Primitive P_Vector) [vectorEntryTypeExpr]) (JS.Array values) =
  cspan (cspan (cline "[") (F.foldr (constructListSpan vectorEntryTypeExpr) CEmpty values)) (cline "]")
renderLiteralValue (TypeExpr (RT_Named (ScopedName{sn_name="Nullable"}, _)) _) JS.Null =
  cline "null"
renderLiteralValue (TypeExpr (RT_Named (ScopedName{sn_name="Nullable"}, _)) [nullableTypeExpr]) (JS.Object object) =
  case HM.lookup "just" object of
    Nothing    -> cline "null"
    Just value -> renderLiteralValue nullableTypeExpr value

renderLiteralValue (TypeExpr (RT_Named (_, Decl{d_type=Decl_Struct struct})) _) (JS.Object value) =
  renderStructLiteralVal struct value

renderLiteralValue (TypeExpr (RT_Named _) _) (JS.String value) = cline ("\'" <> value <> "\'")
renderLiteralValue (TypeExpr (RT_Named _) _) (JS.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Named _) _) (JS.Bool value)
  | value = cline "true"
  | otherwise = cline "false"
renderLiteralValue _ value = cline (T.pack $ show value)


constructListSpan :: CTypeExpr -> JS.Value -> Code -> Code
constructListSpan typeExpr value CEmpty = renderLiteralValue typeExpr value
constructListSpan typeExpr value c2 = CSpan (CSpan (renderLiteralValue typeExpr value) (cline ", ")) c2

renderStructLiteralVal :: CStruct-> JS.Object -> Code
renderStructLiteralVal Struct{s_fields=fields} object =
  cblock "" $ renderLiteralValForFields fields object

renderLiteralValForFields :: [CField] -> JS.Object -> Code
renderLiteralValForFields (x:xs) object = renderLiteralValForField x object <> renderLiteralValForFields xs object
renderLiteralValForFields [] _ = CEmpty

renderLiteralValForField :: CField -> JS.Object -> Code
renderLiteralValForField Field{f_name=fieldName, f_type=fieldType, f_default=defaultValue} object =
  case HM.lookup fieldName object of
    Nothing -> case defaultValue of
      Nothing -> CEmpty
      Just value -> cspan (cspan (ctemplate "$1: " [fieldName]) (renderLiteralValue fieldType value)) (cline ",")
    Just value -> cspan (cspan (ctemplate "$1: " [fieldName]) (renderLiteralValue fieldType value)) (cline ",")

renderModulePrefix :: [Ident] -> T.Text
renderModulePrefix []      = T.pack ""
renderModulePrefix modules = T.intercalate "_" modules <> "."

renderInterface :: InterfaceName -> ParameterNames -> [FieldDetails] -> Bool -> Code
renderInterface name typeParams fields isPrivate =
  if isPrivate then
    cblock (template "interface $1$2" [name, renderedTypeParams]) renderedFields
  else
    cblock (template "export interface $1$2" [name, renderedTypeParams]) renderedFields
  where
    renderedTypeParams = renderParametersExpr typeParams
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
    sortedFds = L.sort fds
    renderedTypeParams = renderParametersExpr typeParams
    factoryInputVariable = renderFactoryInput sortedFds
    factoryDeclaration = template "export function make$1$2" [name, renderedTypeParams]
    fieldInitialisations = renderFieldInitialisations sortedFds
    function = cline (factoryDeclaration <> "(")
      <> indent factoryInputVariable
      <> cline (template "): $1$2 {" [name, renderedTypeParams])
      <> indent fieldInitialisations
      <> cline "}"

renderFactoryInput :: [FieldDetails] -> Code
renderFactoryInput fds = cblock "input:" fields
  where fields = mconcat [renderInputField fd | fd <- fds]
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
    cspan (cspan (ctemplate "$1: input.$1 === undefined ? " [fdName fd]) defaultValue) (ctemplate " : input.$1," [fdName fd])

renderCommentsForDeclaration :: CDecl -> Code
renderCommentsForDeclaration decl = renderComments $ Map.elems (d_annotations decl)

renderComments :: [(CResolvedType, JS.Value)] -> Code
renderComments = L.foldr (CAppend . renderComment) CEmpty

renderComment :: (CResolvedType, JS.Value) -> Code
renderComment (RT_Named (ScopedName{sn_name="Doc"}, _), JS.String commentValue) = clineN commentLinesStarred
  where
    commentLinesStarred = ["/**"] ++ [" * " <> commentLine | commentLine <- commentLinesBroken] ++ [" */"]
    commentLinesBroken = L.filter (/= "") (T.splitOn "\n" commentValue)
renderComment _ = CEmpty

renderParametersExpr :: [T.Text] -> T.Text
renderParametersExpr []         = T.pack ""
renderParametersExpr parameters = "<" <> T.intercalate ", " parameters <> ">"

capitalise :: T.Text -> T.Text
capitalise text = T.cons (C.toUpper (T.head text)) (T.tail text)


addAstDeclaration :: CModule -> CDecl -> CState ()
addAstDeclaration m decl = do
  includeAst <- fmap (cgp_includeAst . mfCodeGenProfile) get
  when includeAst $ do
    let astDecl
          =  ctemplate "const $1 : ADL.ScopedDecl =" [astVariableName decl]
          <> indent (ctemplate "$1;" [jsonToText (scopedDeclAst m decl)])
        typeRef
          =  cblock (template "export function ref$1$2(): ADL.ATypeRef<$1$2>"
                              [d_name decl, renderParametersExpr (getTypeParams (d_type decl))])
                    (ctemplate "return {value : {kind: \"reference\", value : {moduleName : \"$1\",name : \"$2\"}}};"
                               [formatText (m_name m), d_name decl])
    addDeclaration astDecl
    addDeclaration typeRef


astVariableName :: CDecl -> T.Text
astVariableName decl = capitalise (d_name decl) <> "_AST"

addAstMap :: CModule -> CState ()
addAstMap m = do
  addDeclaration $
    cline "export const _AST_MAP = {"
    <> indent (mconcat [ctemplate "\"$1\" : $2$3" [scopedName m decl, astVariableName decl, mcomma]
                       | (decl,mcomma) <- withCommas (Map.elems (m_decls m))])
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
literalAst o@(JS.Object _) = mkUnion "object" o
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
