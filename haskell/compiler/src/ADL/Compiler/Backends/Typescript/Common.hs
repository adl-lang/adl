{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Typescript.Common where

import           ADL.Compiler.AST
import           ADL.Compiler.Backends.Typescript.DataTypes
import           ADL.Compiler.Primitive
import           ADL.Compiler.Processing
import           ADL.Utils.Format                           (template)
import           ADL.Utils.IndentedCode
import           Control.Monad(when)
import           Control.Monad.Trans.State.Strict
import qualified Data.Aeson                                 as JSON
import qualified Data.Char                                  as C
import qualified Data.Foldable                              as F
import qualified Data.HashMap.Lazy                          as HM
import qualified Data.List                                  as L
import qualified Data.Map                                   as Map
import           Data.Monoid
import           Data.Text                                  as T

-- * Stateful functions

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mfDeclarations=code:mfDeclarations mf})

addAstDeclaration :: Code -> CState ()
addAstDeclaration code = do
  state <- get
  when (cgp_includeAst (mfCodeGenProfile state)) (addDeclaration code)

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

genTypeExpr (TypeExpr (RT_Named (ScopedName{sn_moduleName = ModuleName{unModuleName=modules}, sn_name=name}, _)) parameters) = do
    parametersExpressions <- mapM genTypeExpr parameters
    addModulesImport modules name
    return $ (renderModulePrefix modules <> name) <> renderParametersExpr parametersExpressions

genTypeExpr _ = return "TYPE??"  -- FIXME: implement

-- | Generate the typescript literal value for the given ADL type and
-- literal
genLiteralValue :: CTypeExpr -> JSON.Value -> CState Code
genLiteralValue typeExpr value = return $ renderLiteralValue typeExpr value

genAnnotation :: ScopedName -> JSON.Value -> CState T.Text
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

renderLiteralValue :: CTypeExpr -> JSON.Value -> Code
renderLiteralValue (TypeExpr (RT_Primitive P_Double) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Float) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_String) _) (JSON.String value) = ctemplate "'$1'" [value]
renderLiteralValue (TypeExpr (RT_Primitive P_Int8) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Int16) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Int32) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Int64) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Word8) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Word16) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Word32) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Word64) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Primitive P_Bool) _) (JSON.Bool value)
  | value = cline "true"
  | otherwise = cline "false"
renderLiteralValue (TypeExpr (RT_Primitive P_Vector) [vectorEntryTypeExpr]) (JSON.Array values) =
  cspan (cspan (cline "[") (F.foldr (constructListSpan vectorEntryTypeExpr) CEmpty values)) (cline "]")
renderLiteralValue (TypeExpr (RT_Named (ScopedName{sn_name="Nullable"}, _)) _) JSON.Null =
  cline "null"
renderLiteralValue (TypeExpr (RT_Named (ScopedName{sn_name="Nullable"}, _)) [nullableTypeExpr]) (JSON.Object object) =
  case HM.lookup "just" object of
    Nothing    -> cline "null"
    Just value -> renderLiteralValue nullableTypeExpr value

renderLiteralValue (TypeExpr (RT_Named (_, Decl{d_type=Decl_Struct struct})) _) (JSON.Object value) =
  renderStructLiteralVal struct value

renderLiteralValue (TypeExpr (RT_Named _) _) (JSON.String value) = cline ("\'" <> value <> "\'")
renderLiteralValue (TypeExpr (RT_Named _) _) (JSON.Number value) = cline $ litNumber value
renderLiteralValue (TypeExpr (RT_Named _) _) (JSON.Bool value)
  | value = cline "true"
  | otherwise = cline "false"
renderLiteralValue _ value = cline (T.pack $ show value)


constructListSpan :: CTypeExpr -> JSON.Value -> Code -> Code
constructListSpan typeExpr value CEmpty = renderLiteralValue typeExpr value
constructListSpan typeExpr value c2 = CSpan (CSpan (renderLiteralValue typeExpr value) (cline ", ")) c2

renderStructLiteralVal :: CStruct-> JSON.Object -> Code
renderStructLiteralVal Struct{s_fields=fields} object =
  cblock "" $ renderLiteralValForFields fields object

renderLiteralValForFields :: [CField] -> JSON.Object -> Code
renderLiteralValForFields (x:xs) object = renderLiteralValForField x object <> renderLiteralValForFields xs object
renderLiteralValForFields [] _ = CEmpty

renderLiteralValForField :: CField -> JSON.Object -> Code
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

renderComments :: [(CResolvedType, JSON.Value)] -> Code
renderComments = L.foldr (CAppend . renderComment) CEmpty

renderComment :: (CResolvedType, JSON.Value) -> Code
renderComment (RT_Named (ScopedName{sn_name="Doc"}, _), JSON.String commentValue) = clineN commentLinesStarred
  where
    commentLinesStarred = ["/**"] ++ [" * " <> commentLine | commentLine <- commentLinesBroken] ++ [" */"]
    commentLinesBroken = L.filter (/= "") (T.splitOn "\n" commentValue)
renderComment _ = CEmpty

renderParametersExpr :: [T.Text] -> T.Text
renderParametersExpr []         = T.pack ""
renderParametersExpr parameters = "<" <> T.intercalate ", " parameters <> ">"

capitalise :: T.Text -> T.Text
capitalise text = T.cons (C.toUpper (T.head text)) (T.tail text)
