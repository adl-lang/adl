{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Java.Internal where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Parsec as P

import qualified ADL.Adlc.Config.Java as JC
import qualified ADL.Compiler.ParserP as P

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as JSON
import Data.Char(toUpper)
import Data.Maybe(fromMaybe,isJust)
import Data.Foldable(for_,fold)
import Data.List(intersperse,replicate,sort)
import Data.Monoid
import Data.String(IsString(..))
import Data.Traversable(for)

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Backends.Utils.Literals2
import ADL.Compiler.Backends.Utils.IndentedCode
import ADL.Core.Value
import ADL.Utils.Format

data JavaFlags = JavaFlags {
  -- directories where we look for ADL files
  jf_searchPath :: [FilePath],

  -- Files containing custom type definitions
  jf_customTypeFiles :: [FilePath],

  -- The java package under which we hang the generated ADL
  jf_package :: T.Text,
  jf_fileWriter :: FilePath -> LBS.ByteString -> IO (),

  jf_codeGenProfile :: CodeGenProfile
  }

-- Types we want to override
data CustomType = CustomType {
  ct_scopedName :: ScopedName,
  ct_helpers :: ScopedName
  } deriving (Show)

type CustomTypeMap = Map.Map ScopedName CustomType

loadCustomTypes :: [FilePath] -> EIOT CustomTypeMap
loadCustomTypes fps = mconcat <$> sequence [loadFile fp | fp <- fps]
  where
    loadFile :: FilePath -> EIOT CustomTypeMap
    loadFile fp = do
      mv <- liftIO $ aFromJSONFile (jsonSerialiser jsflags) fp
      case mv of
        Nothing -> eioError (template "Unable to read java custom types from  $1" [T.pack fp])
        Just v -> mconcat <$> sequence [convert c | c <- (JC.config_customTypes v)]
    jsflags = JSONFlags True

    convert :: JC.CustomType -> EIOT CustomTypeMap
    convert jct = do
      adlname <- scopedName (JC.customType_adlname jct)
      javaname <- scopedName (JC.customType_javaname jct)
      helpers <- scopedName (JC.customType_helpers jct)
      return (Map.singleton adlname (CustomType javaname helpers))

    scopedName :: T.Text -> EIOT ScopedName
    scopedName t = case P.parse P.scopedName "" t of
          (Right sn) -> return sn
          _ -> eioError (template "Unable to parse scoped name '$1'" [t])

-- A variant of ResolvedType that carries associated custom
-- type information.
type CResolvedType = ResolvedTypeT (Maybe CustomType)

associateCustomTypes :: ModuleName -> CustomTypeMap -> Module ResolvedType -> Module CResolvedType
associateCustomTypes moduleName customTypes mod = fmap assocf mod
  where 
    assocf :: ResolvedType -> CResolvedType
    assocf (RT_Named (sn,decl,()))  = RT_Named (sn,fmap assocf decl,Map.lookup (fullyScope sn) customTypes)
    assocf (RT_Param i) = RT_Param i
    assocf (RT_Primitive pt) = RT_Primitive pt

    fullyScope (ScopedName (ModuleName []) n) = ScopedName moduleName n
    fullyScope sn = sn

newtype JavaPackage = JavaPackage {
  unJavaPackage :: [Ident]
} deriving (Eq)

instance IsString JavaPackage where
  fromString s = JavaPackage (T.splitOn "." (T.pack s))

genJavaPackage :: JavaPackage -> T.Text
genJavaPackage package = T.intercalate "." (map unreserveWord (unJavaPackage package))

genJavaIdentifier :: JavaPackage -> Ident -> T.Text
genJavaIdentifier package name = T.intercalate "." (map unreserveWord (unJavaPackage package) <> [name])

data CodeGenProfile = CodeGenProfile {
  cgp_header :: T.Text,
  cgp_maxLineLength :: Int,
  cgp_mutable :: Bool,
  cgp_hungarianNaming :: Bool,
  cgp_publicMembers :: Bool,
  cgp_genericFactories :: Bool,
  cgp_parcelable :: Bool,
  cgp_runtimePackage :: JavaPackage
}

defaultCodeGenProfile = CodeGenProfile {
  cgp_header = "",
  cgp_mutable = True,
  cgp_maxLineLength = 10000,
  cgp_hungarianNaming = False,
  cgp_publicMembers = False,
  cgp_genericFactories = False,
  cgp_parcelable = False,
  cgp_runtimePackage = "org.adl.runtime"
}

data ClassFile = ClassFile {
   cf_codeProfile :: CodeGenProfile,
   cf_module :: ModuleName,
   cf_javaPackageFn :: ModuleName -> JavaPackage,
   cf_imports :: Map.Map Ident (Maybe JavaPackage),
   cf_implements :: Set.Set T.Text,
   cf_docString :: Code,
   cf_decl :: T.Text,
   cf_fields :: [Code],
   cf_methods :: [Code]
}

classFile :: CodeGenProfile -> ModuleName -> (ModuleName -> JavaPackage) -> T.Text -> ClassFile
classFile codeProfile mname javaPackageFn decl = ClassFile codeProfile mname javaPackageFn Map.empty Set.empty mempty decl [] []

cf_package :: ClassFile -> JavaPackage
cf_package  cf = cf_javaPackageFn cf (cf_module cf)

classFileCode :: ClassFile -> Code
classFileCode content =
  ( if T.null header
      then mempty
      else multiLineComment header <> cline ""
  )
  <>
  ctemplate "package $1;" [genJavaPackage (cf_package content)]
  <>
  cline ""
  <>
  mconcat [ctemplate "import $1;" [imp] | imp <- sortedImports imports]
  <>
  cline ""
  <>
  cf_docString content
  <>
  cblock decl (
    cline ""
    <>
    ( if null (cf_fields content)
        then mempty
        else mconcat [cline "/* Members */" , cline ""]
    )
    <>
    mconcat (reverse (cf_fields content))
    <>
    cline ""
    <>
    mconcat (intersperse (cline "") (reverse (cf_methods content)))
  )
  where
    imports = [genJavaIdentifier package name | (name,Just package) <- Map.toList (cf_imports content)]
    header = cgp_header (cf_codeProfile content)
    decl | Set.null (cf_implements content) = (template "$1" [cf_decl content])
         | otherwise = (template "$1 implements $2" [cf_decl content,commaSep (Set.toList (cf_implements content))])

type CState a = State ClassFile a

addField :: Code -> CState ()
addField decl = modify (\cf->cf{cf_fields=decl:cf_fields cf})

addMethod :: Code -> CState ()
addMethod method = modify (\cf->cf{cf_methods=method:cf_methods cf})

-- | Add an import for an identifer and return the identifier. If there
-- is an inconsistent existing import, return the fully scoped name.
addImport :: JavaPackage -> Ident -> CState T.Text
addImport package name = do
  state <- get 
  case Map.lookup name (cf_imports state) of
    Just mpackage | (Just package) == mpackage -> return name
                  | otherwise                  -> return (genJavaIdentifier package name)
    Nothing -> do
      put state{cf_imports=Map.insert name (Just package) (cf_imports state)}
      return name

preventImport :: Ident -> CState ()
preventImport name = do
  state <- get 
  put state{cf_imports=Map.insert name Nothing (cf_imports state)}
  
addImplements :: T.Text -> CState ()
addImplements imp = modify (\cf->cf{cf_implements=Set.insert imp (cf_implements cf)})

setDocString :: Code -> CState ()
setDocString code = modify (\cf->cf{cf_docString=code})

getRuntimePackage :: CState JavaPackage
getRuntimePackage = (cgp_runtimePackage . cf_codeProfile) <$> get

getHelpers :: CustomType -> CState Ident
getHelpers customType = do
  let (package,name) = javaFromScopedName (ct_helpers customType)
  addImport package name

genTypeExpr :: TypeExpr CResolvedType -> CState T.Text
genTypeExpr te = genTypeExprB False te

genTypeExprB :: Bool ->  TypeExpr CResolvedType -> CState T.Text
genTypeExprB boxed (TypeExpr rt []) = genResolvedType boxed rt
genTypeExprB boxed (TypeExpr rt params) = do
  rtStr <- genResolvedType boxed rt
  rtParamsStr <- mapM (genTypeExprB True) params
  return (template "$1<$2>" [rtStr,T.intercalate ", " rtParamsStr])

genResolvedType :: Bool -> CResolvedType -> CState T.Text
genResolvedType _ (RT_Named (_,_,Just customType)) = do
  let (package,name) = javaFromScopedName (ct_scopedName customType)
  addImport package name
genResolvedType _ (RT_Named (scopedName,_,Nothing)) = genScopedName scopedName
genResolvedType _(RT_Param ident) = return (unreserveWord ident)
genResolvedType False (RT_Primitive pt) = let pd = genPrimitiveDetails pt in fromMaybe (pd_type pd) (pd_unboxed pd)
genResolvedType True (RT_Primitive pt) = pd_type (genPrimitiveDetails pt)

genScopedName :: ScopedName -> CState T.Text
genScopedName scopedName = do
  cf <- get
  let mname = sn_moduleName scopedName
  if mname  == cf_module cf || unModuleName mname == []
    then return (sn_name scopedName)
    else return (T.intercalate "." (unJavaPackage (cf_javaPackageFn cf mname) <> [sn_name scopedName]))

genFactoryExpr :: TypeExpr CResolvedType -> CState T.Text
genFactoryExpr (TypeExpr rt params) = do
  fparams <- mapM genFactoryExpr params
  fe <- case rt of
    (RT_Named (scopedName,_,mct)) -> do
      fscope <- case mct of
        Nothing -> genScopedName scopedName
        (Just ct) -> getHelpers ct
      case params of
        [] -> return (template "$1.FACTORY" [fscope])
        _ -> return (template "$1.factory" [fscope])
    (RT_Param ident) -> return (factoryTypeArg ident)
    (RT_Primitive pt) -> pd_factory (genPrimitiveDetails pt)
  case fparams of
    [] -> return fe
    _ -> return (template "$1($2)" [fe,commaSep fparams])

data PrimitiveDetails = PrimitiveDetails {
  pd_type :: CState T.Text,
  pd_unboxed :: Maybe (CState T.Text),
  pd_default :: Maybe T.Text,
  pd_genLiteral :: JSON.Value -> T.Text,
  pd_mutable :: Bool,
  pd_factory :: CState T.Text,
  pd_hashfn :: T.Text -> T.Text
}

genPrimitiveDetails :: PrimitiveType -> PrimitiveDetails
genPrimitiveDetails P_Void = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "Void",
  pd_default = Just "null",
  pd_genLiteral = \jv -> "null",
  pd_mutable = False,
  pd_factory = primitiveFactory "VOID",
  pd_hashfn = \from -> "0"
  }
genPrimitiveDetails P_Bool = PrimitiveDetails {
  pd_unboxed = Just (return "boolean"),
  pd_type = return "Boolean",
  pd_default = Just "false",
  pd_genLiteral = \jv ->
    case jv of
      (JSON.Bool True) -> "true"
      (JSON.Bool False) -> "false",
  pd_mutable = False,
  pd_factory = primitiveFactory "BOOLEAN",
  pd_hashfn = \from -> template "($1 ? 0 : 1)" [from]
  }
genPrimitiveDetails P_Int8 = PrimitiveDetails {
  pd_unboxed = Just (return "byte"),
  pd_type = return "Byte",
  pd_default = Just "0",
  pd_genLiteral = \(JSON.Number n) -> "(byte)" <> litNumber n,
  pd_mutable = False,
  pd_factory = primitiveFactory "BYTE",
  pd_hashfn = \from -> template "(int) $1" [from]
}
genPrimitiveDetails P_Int16 = PrimitiveDetails {
  pd_unboxed = Just (return "short"),
  pd_type = return "Short",
  pd_default = Just "0",
  pd_genLiteral = \(JSON.Number n) -> "(short)" <> litNumber n,
  pd_mutable = False,
  pd_factory = primitiveFactory "SHORT",
  pd_hashfn = \from -> template "(int) $1" [from]
}
genPrimitiveDetails P_Int32 = PrimitiveDetails {
  pd_unboxed = Just (return "int"),
  pd_type = return "Integer",
  pd_default = Just "0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n,
  pd_mutable = False,
  pd_factory = primitiveFactory "INTEGER",
  pd_hashfn = \from -> template "$1" [from]
}
genPrimitiveDetails P_Int64 = PrimitiveDetails {
  pd_unboxed = Just (return "long"),
  pd_type = return "Long",
  pd_default = Just "0L",
  pd_genLiteral = \(JSON.Number n) -> litNumber n <> "L",
  pd_mutable = False,
  pd_factory = primitiveFactory "LONG",
  pd_hashfn = \from -> template "(int) ($1 ^ ($1 >>> 32))" [from]
}
genPrimitiveDetails P_Float = PrimitiveDetails {
  pd_unboxed = Just (return "float"),
  pd_type = return "Float",
  pd_default = Just "0.0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n <> "F",
  pd_mutable = False,
  pd_factory = primitiveFactory "FLOAT",
  pd_hashfn = \from -> template "Float.valueOf($1).hashCode()" [from]
}
genPrimitiveDetails P_Double = PrimitiveDetails {
  pd_unboxed = Just (return "double"),
  pd_type = return "Double",
  pd_default = Just "0.0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n,
  pd_mutable = False,
  pd_factory = primitiveFactory "DOUBLE",
  pd_hashfn = \from -> template "Double.valueOf($1).hashCode()" [from]
}

genPrimitiveDetails P_Word8 = genPrimitiveDetails P_Int8
genPrimitiveDetails P_Word16 = genPrimitiveDetails P_Int16
genPrimitiveDetails P_Word32 = genPrimitiveDetails P_Int32
genPrimitiveDetails P_Word64 = genPrimitiveDetails P_Int64

genPrimitiveDetails P_ByteVector = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = do
    rtpackage <- getRuntimePackage
    addImport rtpackage "ByteArray",
  pd_default = Just "new ByteArray()",
  pd_genLiteral = \(JSON.String s) -> template "new ByteArray($1.getBytes())" [T.pack (show (decode s))],
  pd_mutable = True,
  pd_factory = primitiveFactory "BYTE_ARRAY",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
  where
    decode s = case B64.decode (T.encodeUtf8 s) of
      (Left _) -> "???"
      (Right s) -> s
genPrimitiveDetails P_Vector = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = addImport "java.util" "ArrayList",
  pd_default = Nothing,
  pd_genLiteral = \(JSON.String s) -> "???", -- never called
  pd_mutable = True,
  pd_factory = primitiveFactory "arrayList",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_String = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "String",
  pd_default = Just "\"\"",
  pd_genLiteral = \(JSON.String s) -> T.pack (show s),
  pd_mutable= False,
  pd_factory = primitiveFactory "STRING",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_Sink = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "Sink",
  pd_default = Just "new Sink()",
  pd_genLiteral = \_ -> "????", -- never called
  pd_mutable = True,
  pd_factory = primitiveFactory "SINK",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }

primitiveFactory :: T.Text -> CState T.Text
primitiveFactory name = do
  rtpackage <- getRuntimePackage
  factories <- addImport rtpackage "Factories"
  return (template "$1.$2" [factories,name])

data FieldDetails = FieldDetails {
  fd_field :: Field CResolvedType,
  fd_fieldName :: Ident,
  fd_typeExprStr :: T.Text,
  fd_boxedTypeExprStr :: T.Text,
  fd_factoryExprStr :: T.Text,
  fd_defValue :: T.Text,
  fd_copy :: T.Text -> T.Text
}

unboxedField fd = case (f_type (fd_field fd)) of
  (TypeExpr (RT_Primitive pt) []) -> isJust (pd_unboxed (genPrimitiveDetails pt))
  _ -> False

needsNullCheck fd = not (unboxedField fd || fd_typeExprStr fd == "Void")

immutableType te = case te of
  (TypeExpr (RT_Primitive pt) _) -> not (pd_mutable (genPrimitiveDetails pt))
  _-> False

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails f = do
  let te = f_type f
  typeExprStr <- genTypeExprB False te
  boxedTypeExprStr <- genTypeExprB True te
  factoryExprStr <- genFactoryExpr te
  litv <- case f_default f of
    (Just v) -> case mkLiteral te v of
      Left e -> error ("BUG: invalid json literal: " ++ T.unpack e)
      Right litv -> return litv
    Nothing -> return (LDefault te)
  defValue <- genLiteralText litv
  cgp <- cf_codeProfile <$> get

  let copy from =
        if immutableType te
          then from
          else template "$1.create($2)" [factoryExprStr,from]
      hungarian = cgp_hungarianNaming cgp && not (cgp_publicMembers cgp)
      fieldName = if hungarian then "m" <> javaCapsFieldName f else unreserveWord (f_name f)

  return FieldDetails {
    fd_field=f,
    fd_fieldName=fieldName,
    fd_typeExprStr=typeExprStr,
    fd_boxedTypeExprStr=boxedTypeExprStr,
    fd_factoryExprStr=factoryExprStr,
    fd_defValue=defValue,
    fd_copy=copy
    }

-- Inside the union implementation we need to be able to cast
-- from Object to the type of the branch. For a simple enough type
-- (T) v is enough. When generics are involved we need to call
-- a helper function to suppress the warnings.
    
needsSuppressedCheckInCast :: TypeExpr CResolvedType -> Bool
needsSuppressedCheckInCast (TypeExpr (RT_Param _) []) = True
needsSuppressedCheckInCast (TypeExpr _ []) = False
needsSuppressedCheckInCast _ = True


generateDocString :: Annotations -> Code
generateDocString annotations = case Map.lookup (ScopedName (ModuleName []) "Doc") annotations of
   (Just (JSON.String text)) -> docStringComment text
   _ -> mempty

docStringComment :: T.Text -> Code
docStringComment text = cline "/**" <> mconcat [cline (" * " <> line) | line <- T.lines text] <> cline " */"

multiLineComment :: T.Text -> Code
multiLineComment text = cline "/*" <> mconcat [cline (" * " <> line) | line <- T.lines text] <> cline " */"

genLiteralText :: Literal (TypeExpr CResolvedType) -> CState T.Text
genLiteralText (LDefault (TypeExpr (RT_Named (_,_,Just customType)) [])) = do
  idHelpers <- getHelpers customType
  return (template "$1.FACTORY.create()" [idHelpers])
genLiteralText (LDefault te@(TypeExpr (RT_Primitive pt) _)) = do
  case  pd_default (genPrimitiveDetails pt) of
    Just defaultStr -> return defaultStr
    Nothing -> do
      typeExpr <- genTypeExpr te
      return (template "new $1()" [typeExpr])
genLiteralText (LDefault te@(TypeExpr _ [])) = do
  typeExpr <- genTypeExpr te
  return (template "new $1()" [typeExpr])
genLiteralText (LDefault te) = do
  factoryExpr <- genFactoryExpr te
  return (template "$1.create()" [factoryExpr])
genLiteralText (LCtor (TypeExpr (RT_Named (_,_,Just customType)) _) ls) = do
  idHelpers <- getHelpers customType
  lits <- mapM genLiteralText ls
  return (template "$1.create($2)" [idHelpers, T.intercalate ", " lits])
genLiteralText (LCtor te ls) = do
  typeExpr <- genTypeExpr te
  lits <- mapM genLiteralText ls
  return (template "new $1($2)" [typeExpr, T.intercalate ", " lits])
genLiteralText (LUnion (TypeExpr (RT_Named (_,_,Just customType)) _) ctor l) = do
  idHelpers <- getHelpers customType
  lit <- genLiteralText l
  return (template "$1.$2($3)" [idHelpers, ctor, lit ])
genLiteralText (LUnion te ctor l) = do
  typeExpr <- genTypeExpr te
  lit <- genLiteralText l
  return (template "$1.$2($3)" [typeExpr, ctor, lit ])
genLiteralText (LVector _ ls) = do
  lits <- mapM genLiteralText ls
  arrays <- addImport "java.util" "Arrays"
  return (template "$1.asList($2)" [arrays,commaSep lits])
genLiteralText (LPrimitive pt jv) = do
  return (pd_genLiteral (genPrimitiveDetails pt) jv)

packageGenerator :: T.Text -> ModuleName -> JavaPackage
packageGenerator basePackage mn = JavaPackage (T.splitOn "." basePackage <> unModuleName mn)

fileGenerator :: T.Text -> ScopedName -> FilePath
fileGenerator basePackage sn = T.unpack (T.intercalate "/" idents <> ".java")
  where
    idents = unJavaPackage (packageGenerator basePackage (sn_moduleName sn)) <> [sn_name sn]

commaSep :: [T.Text] -> T.Text
commaSep = T.intercalate ", "

sortedImports :: [T.Text] -> [T.Text]
sortedImports imports = map snd (sort [(classify i,i) | i <- imports])
  where
    classify i = if T.isPrefixOf "java." i
                   then 2
                   else if T.isPrefixOf "android." i then 1 else 0

----------------------------------------------------------------------
reservedWords :: Set.Set Ident
reservedWords = Set.fromList
 [ "abstract"
 , "assert"
 , "boolean"
 , "break"
 , "byte"
 , "case"
 , "catch"
 , "char"
 , "class"
 , "const"
 , "continue"
 , "default"
 , "do"
 , "double"
 , "else"
 , "enum"
 , "extends"
 , "false"
 , "final"
 , "finally"
 , "float"
 , "for"
 , "goto"
 , "if"
 , "implements"
 , "import"
 , "instanceof"
 , "int"
 , "interface"
 , "long"
 , "native"
 , "new"
 , "null"
 , "package"
 , "private"
 , "protected"
 , "public"
 , "return"
 , "short"
 , "static"
 , "strictfp"
 , "super"
 , "switch"
 , "synchronized"
 , "this"
 , "throw"
 , "throws"
 , "transient"
 , "true"
 , "try"
 , "void"
 , "volatile"
 , "while"

 -- reserved for ADL  
 , "factory"
 ]

unreserveWord :: Ident -> Ident
unreserveWord n | Set.member n reservedWords = T.append n "_"
                | otherwise = n

javaCapsFieldName :: Field CResolvedType -> Ident
javaCapsFieldName f = case T.uncons (f_name f) of
  Nothing -> ""
  (Just (c,t)) -> T.cons (toUpper c) t

factoryTypeArg :: Ident -> Ident
factoryTypeArg n = "factory" <> n

fieldAccessExpr :: CodeGenProfile -> FieldDetails -> Ident
fieldAccessExpr cgp fd
  | cgp_publicMembers cgp = fd_fieldName fd
  | otherwise = template "get$1()" [javaCapsFieldName (fd_field fd)]

unionCtorName :: Field a -> Ident
unionCtorName f = unreserveWord (f_name f)

javaFromScopedName :: ScopedName -> (JavaPackage,Ident)
javaFromScopedName scopedName = (JavaPackage (unModuleName (sn_moduleName scopedName)),sn_name scopedName)

discriminatorName :: FieldDetails -> Ident
discriminatorName = T.toUpper . unreserveWord . f_name . fd_field

leadSpace :: T.Text -> T.Text
leadSpace "" = ""
leadSpace t = " " <> t
