{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Javascript(
 generate,
  JavascriptFlags(..),
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.Text.Encoding as T

import Control.Monad(when)
import Control.Monad.Trans(liftIO)
import Control.Monad.Trans.State.Strict(State,execState,modify,get,put)
import Data.Aeson( (.=) )
import Data.Monoid
import Data.Foldable(for_)
import System.FilePath(joinPath,(</>),(<.>))

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.DataFiles
import ADL.Compiler.Primitive
import ADL.Compiler.Processing
import ADL.Compiler.Utils
import ADL.Core.Value
import ADL.Utils.FileDiff(dirContents)
import ADL.Utils.Format
import ADL.Utils.IndentedCode

-- | Command line flags to control the backend.
-- (once we have them)

data JavascriptFlags = JavascriptFlags

-- A variant of the AST that carries custom type
-- information.

type CResolvedType = ResolvedTypeT (Maybe CustomType)
type CTypeExpr = TypeExpr (CResolvedType)
type CModule = Module (Maybe CustomType) CResolvedType
type CDecl = Decl (Maybe CustomType) CResolvedType

-- currently we don't support custom types, but when we do,
-- they would go here (see the java backend as an example)

data CustomType = CustomType
  deriving (Show)

-- We use a state monad to accumulate the javascript file corresponding
-- to each ADL module
type CState a = State ModuleFile a

data ModuleFile = ModuleFile {
  mf_moduleName :: ModuleName,

  -- The modules on which this one depends
  mf_depends :: Set.Set ModuleName,

  -- The code
  mf_declarations :: [Code],

  -- The exports
  mf_exports :: [ScopedName]
}

data JSModuleName = JSModuleName {
  mn_package :: [Ident],
  mn_name :: Ident
} deriving (Eq,Ord,Show)


 -- Details we capture on each field
data FieldDetails = FieldDetails {
  fd_field :: Field CResolvedType,
  fd_name :: Ident,
  fd_typeExpr :: JSON.Value
  }

-- | The key functions needed to plug a type into the
-- code generator
data TypeDetails = TypeDetails {

  -- | Generate the json representation of the type,
  -- given the representation of the type arguments.
  td_type :: [JSON.Value] -> CState JSON.Value
}

emptyModuleFile :: ModuleName -> ModuleFile
emptyModuleFile mn = ModuleFile mn Set.empty [] []

generate :: AdlFlags -> JavascriptFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af jf fileWriter modulePaths = catchAllExceptions  $ do
    for_ modulePaths $ \modulePath -> do
      m <- loadAndCheckModule af modulePath
      generateModule jf fileWriter m

-- | Generate and write the javascript code for a single ADL module
generateModule :: JavascriptFlags ->
                  FileWriter ->
                  RModule ->
                  EIO T.Text ()
generateModule jf fileWriter m0 = do
  let moduleName = m_name m
      m = associateCustomTypes getCustomType moduleName m0
      decls = Map.elems (m_decls m)
      mf = execState (genModule m) (emptyModuleFile (m_name m))
  liftIO $ fileWriter (jsModuleFilePath (jsModuleName moduleName)) (jsModuleCode mf)

genModule :: CModule -> CState ()
genModule m = do
  -- Generate each declaration
  for_ (Map.elems (m_decls m)) $ \decl -> do
    case d_type decl of
     (Decl_Struct struct) -> genStruct m decl struct
     (Decl_Union union) -> genUnion' m decl union
     (Decl_Typedef typedef) -> genTypedef m decl typedef
     (Decl_Newtype ntype) -> genNewtype m decl ntype

genStruct :: CModule -> CDecl -> Struct CResolvedType -> CState ()
genStruct  m decl struct = genAType m decl (s_typeParams struct) (s_fields struct) "struct"

genUnion' :: CModule -> CDecl -> Union CResolvedType -> CState ()
genUnion' m decl union = genAType m decl (u_typeParams union) (u_fields union) "union"

genAType :: CModule -> CDecl -> [Ident] -> [Field CResolvedType] -> Ident -> CState ()
genAType mod decl typeParams fields kind = do
  fieldDetails <- mapM genFieldDetails fields
  let jsname = unreserveWord (d_name decl)
      code
        = ctemplate "const $1 = {" [jsname]
        <> indent
          (  ctemplate "name : \"$1\"," [d_name decl]
          <> ctemplate "module : \"$1\"," [formatText (m_name mod)]
          <> ctemplate "kind : \"$1\"," [kind]
          <> ctemplate "typevars : [$1]," [textFromTypeVars typeParams]
          <> cline "fields : ["
          <> indent (mconcat [mkField f comma | (f,comma) <- withCommas fieldDetails])
          <> cline "],"
          <> cline "annotations : ["
          <> indent (mconcat [mkAnnotation mod a comma | (a,comma) <- (withCommas . Map.toList) (d_annotations decl)])
          <> cline "]"
          )
        <> cline "};"
      mkField fieldDetails mcomma =
        cline "{"
        <> indent
          (  ctemplate "name : \"$1\"," [f_name field]
          <> ctemplate "type : $1," [textFromJson (fd_typeExpr fieldDetails)]
          <> ctemplate "defaultv : $1," [textFromDefault (f_default field)]
          <> cline "annotations : ["
          <> indent (mconcat [mkAnnotation mod a comma | (a,comma) <- (withCommas . Map.toList) (f_annotations field)])
          <> cline "]"
          )
        <> ctemplate "}$1" [mcomma]
        where
          field = fd_field fieldDetails

  addDeclaration code
  addExport (ScopedName (m_name mod) (d_name decl))

textFromDefault :: Maybe JSON.Value -> T.Text
textFromDefault Nothing = "null"
textFromDefault (Just v) = textFromJson v

textFromJson :: JSON.Value -> T.Text
textFromJson = LT.toStrict . LT.toLazyText . JSON.encodeToTextBuilder

textFromTypeVars :: [Ident] -> T.Text
textFromTypeVars tvars = template "[$1]" [T.intercalate "," ["\"" <> tvar <> "\"" | tvar <- tvars]]

genTypedef :: CModule -> CDecl -> Typedef CResolvedType -> CState ()
genTypedef  mod decl typedef = do
  texpr <- genTypeExpr (t_typeExpr typedef)
  let jsname = unreserveWord (d_name decl)
      code
        =  ctemplate "const $1 = {" [jsname]
        <> indent
          (  ctemplate "name : \"$1\"," [d_name decl]
          <> ctemplate "module : \"$1\"," [formatText (m_name mod)]
          <> cline "kind : \"typedef\","
          <> ctemplate "typevars : $1," [textFromTypeVars (t_typeParams typedef)]
          <> ctemplate "type : $1," [textFromJson texpr]
          <> cline "annotations : ["
          <> indent (mconcat [mkAnnotation mod a comma | (a,comma) <- (withCommas . Map.toList) (d_annotations decl)])
          <> cline "]"
          )
        <> cline "};"

  addDeclaration code
  addExport (ScopedName (m_name mod) (d_name decl))

genNewtype :: CModule -> CDecl -> Newtype CResolvedType -> CState ()
genNewtype  mod decl ntype = do
  texpr <- genTypeExpr (n_typeExpr ntype)
  let jsname = unreserveWord (d_name decl)
      code
        =  ctemplate "const $1 = {" [jsname]
        <> indent
          (  ctemplate "name : \"$1\"," [d_name decl]
          <> ctemplate "module : \"$1\"," [formatText (m_name mod)]
          <> cline "kind : \"newtype\","
          <> ctemplate "typevars : $1," [textFromTypeVars (n_typeParams ntype)]
          <> ctemplate "type : $1," [textFromJson texpr]
          <> ctemplate "defaultv : $1," [textFromDefault (n_default ntype)]
          <> cline "annotations : ["
          <> indent (mconcat [mkAnnotation mod a comma | (a,comma) <- (withCommas . Map.toList) (d_annotations decl)])
          <> cline "]"
          )
        <> cline "};"

  addDeclaration code
  addExport (ScopedName (m_name mod) (d_name decl))

mkAnnotation :: CModule -> (ScopedName,(r,JSON.Value)) -> T.Text -> Code
mkAnnotation mod (ann,(_,value)) mcomma =
  cline "{"
    <> indent
      (  ctemplate "type : \"$1\"," [formatText (scopedName (m_name mod) ann)]
      <> ctemplate "value : $1" [textFromJson value]
      )
    <> ctemplate "}$1" [mcomma]

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails field = do
  typeExprStr <- genTypeExpr (f_type field)
  return (FieldDetails field (f_name field) typeExprStr)

genTypeExpr :: CTypeExpr -> CState JSON.Value
genTypeExpr (TypeExpr rt params) = do
  rtParams <- mapM genTypeExpr params
  td_type (getTypeDetails rt) rtParams

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType scopedName decl = Nothing

addDeclaration :: Code -> CState ()
addDeclaration code = modify (\mf->mf{mf_declarations=code:mf_declarations mf})

addExport :: ScopedName -> CState ()
addExport sn = modify (\mf->mf{mf_exports=sn:mf_exports mf})

addDepends :: ModuleName -> CState ()
addDepends moduleName = do
  modify (\mf->mf{mf_depends=Set.insert moduleName (mf_depends mf)})

getTypeDetails ::CResolvedType -> TypeDetails

-- a type defined through a regular ADL declaration
getTypeDetails rt@(RT_Named (scopedName,Decl{d_customType=Nothing})) = TypeDetails
  { td_type = \typeArgs -> do
      ts <- genScopedName scopedName
      return (withTypeArgs ts typeArgs)
  }

-- a custom type
getTypeDetails rt@(RT_Named (_,Decl{d_customType=Just customType})) = undefined

-- a type variable
getTypeDetails (RT_Param typeVar) = TypeDetails
  { td_type = \_ -> return (JSON.object ["var" .= typeVar])
  }

-- each primitive
getTypeDetails (RT_Primitive P_Void) = monomorphicPrimitive "Void"
getTypeDetails (RT_Primitive P_String) = monomorphicPrimitive "String"
getTypeDetails (RT_Primitive P_Int8) = monomorphicPrimitive "Int8"
getTypeDetails (RT_Primitive P_Int16) = monomorphicPrimitive "Int16"
getTypeDetails (RT_Primitive P_Int32) = monomorphicPrimitive "Int32"
getTypeDetails (RT_Primitive P_Int64) = monomorphicPrimitive "Int64"
getTypeDetails (RT_Primitive P_Word8) = monomorphicPrimitive "Word8"
getTypeDetails (RT_Primitive P_Word16) = monomorphicPrimitive "Word16"
getTypeDetails (RT_Primitive P_Word32) = monomorphicPrimitive "Word32"
getTypeDetails (RT_Primitive P_Word64) = monomorphicPrimitive "Word64"
getTypeDetails (RT_Primitive P_Double) = monomorphicPrimitive "Double"
getTypeDetails (RT_Primitive P_Float) = monomorphicPrimitive "Double"
getTypeDetails (RT_Primitive P_Bool) = monomorphicPrimitive "Bool"
getTypeDetails (RT_Primitive P_Json) = monomorphicPrimitive "Json"
getTypeDetails (RT_Primitive P_ByteVector) = monomorphicPrimitive "ByteVector"
getTypeDetails (RT_Primitive P_Vector) = polymorphicPrimitive "Vector"
getTypeDetails (RT_Primitive P_StringMap) = polymorphicPrimitive "StringMap"
getTypeDetails (RT_Primitive P_Nullable) = polymorphicPrimitive "Nullable"
getTypeDetails (RT_Primitive P_Json) = monomorphicPrimitive "Json"

monomorphicPrimitive :: T.Text -> TypeDetails
monomorphicPrimitive name = TypeDetails
  { td_type = \_ -> return (JSON.object ["primitive" .= name])
  }

polymorphicPrimitive :: T.Text  -> TypeDetails
polymorphicPrimitive name = TypeDetails
  { td_type = \typeArgs -> do
      return (withTypeArgs (JSON.object ["primitive" .= name]) typeArgs)
  }

scopedName :: ModuleName -> ScopedName -> ScopedName
scopedName mname sn@ScopedName{sn_moduleName=ModuleName []} = sn{sn_moduleName=mname}
scopedName _ sn = sn

genScopedName :: ScopedName -> CState JSON.Value
genScopedName sn0 = do
  mf <- get
  let sn = scopedName (mf_moduleName mf) sn0
  when (sn_moduleName sn /= mf_moduleName mf) (addDepends (sn_moduleName sn))
  return (JSON.object ["ref" .= formatText (scopedName (mf_moduleName mf) sn)])

withTypeArgs :: JSON.Value -> [JSON.Value] -> JSON.Value
withTypeArgs ts [] = ts
withTypeArgs ts tsargs = JSON.object ["app" .= ts, "args" .= tsargs]

  -- template "$1($2)" [ts,T.intercalate ", " tsargs]


-- | Convert an ADL module name to a javascript one
jsModuleName :: ModuleName -> JSModuleName
jsModuleName (ModuleName ids) = JSModuleName (reverse rest) last
  where
    (last:rest) = reverse ids

jsModuleFilePath  :: JSModuleName -> FilePath
jsModuleFilePath (JSModuleName path name) = joinPath (map T.unpack path) </> T.unpack name <.> "js"

jsModuleCode :: ModuleFile -> LBS.ByteString
jsModuleCode mf = LBS.fromStrict (T.encodeUtf8 (T.unlines (codeText 10000 code)))
  where
    code =  ctemplate "// Automatically generated code from adl: $1" [formatText (mf_moduleName mf)]
         <> cline "//"
         <> mconcat [ctemplate "// depends on adl: $1" [formatText mn] | mn <- Set.toList (mf_depends mf)]
         <> cline "//"
         <> cline ""
         <> mconcat [ CLine "" <> decl | decl <- reverse (mf_declarations mf)]
         <> cline ""
         <> cline "export const _ADL_TYPES = {"
         <> indent (mconcat [ctemplate "\"$1\" : $2$3" [formatText sn, unreserveWord (sn_name sn), mcomma]
                            | (sn,mcomma) <- withCommas (mf_exports mf)])
         <> cline "};"

coreModule :: JSModuleName
coreModule = JSModuleName ["adl"] "core"

reservedWords :: Set.Set Ident
reservedWords = Set.fromList
 [ "abstract"
 , "else"
 , "instanceof"
 , "super"
 , "boolean"
 , "enum"
 , "int"
 , "switch"
 , "break"
 , "export"
 , "interface"
 , "synchronized"
 , "byte"
 , "extends"
 , "let"
 , "this"
 , "case"
 , "false"
 , "long"
 , "throw"
 , "catch"
 , "final"
 , "native"
 , "throws"
 , "char"
 , "finally"
 , "new"
 , "transient"
 , "class"
 , "float"
 , "null"
 , "true"
 , "const"
 , "for"
 , "package"
 , "try"
 , "continue"
 , "function"
 , "private"
 , "typeof"
 , "debugger"
 , "goto"
 , "protected"
 , "var"
 , "default"
 , "if"
 , "public"
 , "void"
 , "delete"
 , "implements"
 , "return"
 , "volatile"
 , "do"
 , "import"
 , "short"
 , "while"
 , "double"
 , "in"
 , "static"
 , "with"
 ]

unreserveWord :: Ident -> Ident
unreserveWord n | Set.member n reservedWords = T.append n "_"
                | otherwise = n

withCommas :: [a] -> [(a,T.Text)]
withCommas [] = []
withCommas [a] = [(a,"")]
withCommas (a:as) = (a,","):withCommas as
