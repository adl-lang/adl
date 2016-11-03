{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Java.Internal where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Scientific as S
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
import ADL.Compiler.Backends.Utils.IndentedCode
import ADL.Core.Value
import ADL.Utils.Format

data JavaFlags = JavaFlags {
  jf_libDir :: FilePath,
  
  -- Files containing custom type definitions
  jf_customTypeFiles :: [FilePath],

  -- The java package under which we hang the generated ADL
  jf_package :: JavaPackage,

  -- Whether to include the runtime in the generated output
  jf_includeRuntime :: Bool,

  jf_codeGenProfile :: CodeGenProfile
  }

-- A type for (part of) a java package name
newtype JavaPackage = JavaPackage [Ident]
  deriving (Eq,Ord,Show)

javaPackage :: T.Text -> JavaPackage
javaPackage s = JavaPackage (map unreserveWord (T.splitOn "." s ))

instance Monoid JavaPackage where
  mempty = JavaPackage []
  mappend (JavaPackage p1) (JavaPackage p2) = JavaPackage (p1++p2)

instance IsString JavaPackage where
  fromString s = JavaPackage (T.splitOn "." (T.pack s))

-- A type for a fully scoped java class name
newtype JavaClass = JavaClass [Ident]
  deriving (Eq,Ord,Show)

instance IsString JavaClass where
  fromString s = JavaClass (T.splitOn "." (T.pack s))

javaClass :: JavaPackage -> Ident -> JavaClass
javaClass (JavaPackage p1) name = JavaClass (p1++[unreserveWord name])

splitJavaClass :: JavaClass -> (JavaPackage,Ident)
splitJavaClass (JavaClass ids) = (JavaPackage (init ids),last ids)

withPackagePrefix :: JavaPackage -> JavaClass -> JavaClass
withPackagePrefix (JavaPackage ids1) (JavaClass ids2) = JavaClass (ids1++ids2)

genJavaPackage :: JavaPackage -> T.Text
genJavaPackage (JavaPackage ids) = T.intercalate "." ids

genJavaClass :: JavaClass -> T.Text
genJavaClass (JavaClass ids) = T.intercalate "." ids

javaClassFilePath :: JavaClass -> FilePath
javaClassFilePath (JavaClass ids) = T.unpack (T.intercalate "/" ids <> ".java")

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
associateCustomTypes moduleName customTypes mod = mapModule assocf mod
  where 
    assocf :: ResolvedType -> CResolvedType
    assocf (RT_Named (sn,decl,()))  = RT_Named (sn,mapDecl assocf decl,Map.lookup (fullyScope sn) customTypes)
    assocf (RT_Param i) = RT_Param i
    assocf (RT_Primitive pt) = RT_Primitive pt

    fullyScope (ScopedName (ModuleName []) n) = ScopedName moduleName n
    fullyScope sn = sn

data CodeGenProfile = CodeGenProfile {
  cgp_header :: T.Text,
  cgp_maxLineLength :: Int,
  cgp_mutable :: Bool,
  cgp_hungarianNaming :: Bool,
  cgp_publicMembers :: Bool,
  cgp_genericFactories :: Bool,
  cgp_json :: Bool,
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
  cgp_json = False,
  cgp_parcelable = False,
  cgp_runtimePackage = javaPackage "org.adl.runtime"
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
  ctemplate "package $1;" [genJavaPackage javaPackage]
  <>
  cline ""
  <>
  mconcat [ctemplate "import $1;" [genJavaClass imp] | imp <- sortedImports imports]
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
    javaPackage = cf_package content
    imports = [javaClass package name | (name,Just package) <- Map.toList (cf_imports content), package /= javaPackage]
    header = cgp_header (cf_codeProfile content)
    decl | Set.null (cf_implements content) = (template "$1" [cf_decl content])
         | otherwise = (template "$1 implements $2" [cf_decl content,commaSep (Set.toList (cf_implements content))])

type CState a = State ClassFile a

addField :: Code -> CState ()
addField decl = modify (\cf->cf{cf_fields=decl:cf_fields cf})

addMethod :: Code -> CState ()
addMethod method = modify (\cf->cf{cf_methods=method:cf_methods cf})

-- | Add an import for a java class and return the identifier. If there
-- is an inconsistent existing import, return the fully scoped name.
addImport :: JavaClass -> CState T.Text
addImport cls  = do
  let (package,name) = splitJavaClass cls
  state <- get 
  case Map.lookup name (cf_imports state) of
    Just mpackage | (Just package) == mpackage -> return name
                  | otherwise                  -> return (genJavaClass cls)
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
  jclass <- fixStdRef (classFromScopedName (ct_helpers customType))
  addImport jclass

data TypeBoxed = TypeBoxed | TypeUnboxed

genTypeExpr :: TypeExpr CResolvedType -> CState T.Text
genTypeExpr te = genTypeExprB TypeUnboxed te

genTypeExprB :: TypeBoxed ->  TypeExpr CResolvedType -> CState T.Text
genTypeExprB boxed (TypeExpr rt params) = do
  rtParamsStr <- mapM (genTypeExprB TypeBoxed) params
  genResolvedType boxed rt rtParamsStr

genResolvedType :: TypeBoxed -> CResolvedType -> [T.Text] -> CState T.Text
genResolvedType _ (RT_Named (_,_,Just customType)) args = do
  ts <- addImport (classFromScopedName (ct_scopedName customType))
  return (withTypeArgs ts args)
genResolvedType _ (RT_Named (scopedName,_,Nothing)) args = do
  ts <- genScopedName scopedName
  return (withTypeArgs ts args)
genResolvedType _(RT_Param ident) args = return (unreserveWord ident)
genResolvedType boxed (RT_Primitive pt) args = do
  pd_type (genPrimitiveDetails pt) boxed args

withTypeArgs :: T.Text -> [T.Text] -> T.Text
withTypeArgs ts [] = ts
withTypeArgs ts tsargs = template "$1<$2>" [ts,T.intercalate ", " tsargs] 

genScopedName :: ScopedName -> CState T.Text
genScopedName scopedName0 = do
  cf <- get
  let scopedName =
        case sn_moduleName scopedName0 of
          (ModuleName []) -> ScopedName (cf_module cf) (sn_name scopedName0)
          _ -> scopedName0
  addImport (javaClass (cf_javaPackageFn cf (sn_moduleName scopedName)) (sn_name scopedName))

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
  pd_type :: TypeBoxed -> [T.Text] -> CState T.Text,
  pd_default :: Maybe T.Text,
  pd_genLiteral :: JSON.Value -> T.Text,
  pd_mutable :: Bool,
  pd_factory :: CState T.Text,
  pd_hashfn :: T.Text -> T.Text
}

genPrimitiveDetails :: PrimitiveType -> PrimitiveDetails
genPrimitiveDetails P_Void = PrimitiveDetails {
  pd_type = unboxedPrimitive "Void" "Void",
  pd_default = Just "null",
  pd_genLiteral = \jv -> "null",
  pd_mutable = False,
  pd_factory = primitiveFactory "VOID",
  pd_hashfn = \from -> "0"
  }
genPrimitiveDetails P_Bool = PrimitiveDetails {
  pd_type = unboxedPrimitive "boolean" "Boolean",
  pd_default = Just "false",
  pd_genLiteral = \jv ->
    case jv of
      (JSON.Bool True) -> "true"
      (JSON.Bool False) -> "false"
      _ -> error "BUG: invalid literal type for P_Bool",
  pd_mutable = False,
  pd_factory = primitiveFactory "BOOLEAN",
  pd_hashfn = \from -> template "($1 ? 0 : 1)" [from]
  }
genPrimitiveDetails P_Int8 = PrimitiveDetails {
  pd_type = unboxedPrimitive "byte" "Byte",
  pd_default = Just "(byte)0",
  pd_genLiteral = \(JSON.Number n) -> "(byte)" <> litNumber n,
  pd_mutable = False,
  pd_factory = primitiveFactory "BYTE",
  pd_hashfn = \from -> template "(int) $1" [from]
}
genPrimitiveDetails P_Int16 = PrimitiveDetails {
  pd_type = unboxedPrimitive "short" "Short",
  pd_default = Just "(short)0",
  pd_genLiteral = \(JSON.Number n) -> "(short)" <> litNumber n,
  pd_mutable = False,
  pd_factory = primitiveFactory "SHORT",
  pd_hashfn = \from -> template "(int) $1" [from]
}
genPrimitiveDetails P_Int32 = PrimitiveDetails {
  pd_type = unboxedPrimitive "int" "Integer",
  pd_default = Just "0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n,
  pd_mutable = False,
  pd_factory = primitiveFactory "INTEGER",
  pd_hashfn = \from -> template "$1" [from]
}
genPrimitiveDetails P_Int64 = PrimitiveDetails {
  pd_type = unboxedPrimitive "long" "Long",
  pd_default = Just "0L",
  pd_genLiteral = \(JSON.Number n) -> litNumber n <> "L",
  pd_mutable = False,
  pd_factory = primitiveFactory "LONG",
  pd_hashfn = \from -> template "(int) ($1 ^ ($1 >>> 32))" [from]
}
genPrimitiveDetails P_Float = PrimitiveDetails {
  pd_type = unboxedPrimitive "float" "Float",
  pd_default = Just "0.0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n <> "F",
  pd_mutable = False,
  pd_factory = primitiveFactory "FLOAT",
  pd_hashfn = \from -> template "Float.valueOf($1).hashCode()" [from]
}
genPrimitiveDetails P_Double = PrimitiveDetails {
  pd_type = unboxedPrimitive "double" "Double",
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
  pd_type = \_ _ -> do
    rtpackage <- getRuntimePackage
    addImport (javaClass rtpackage "ByteArray"),
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
  pd_type = \_ args -> do
     arrayListI <- addImport "java.util.ArrayList"
     return (withTypeArgs arrayListI args),
  pd_default = Nothing,
  pd_genLiteral = \(JSON.String s) -> "???", -- never called
  pd_mutable = True,
  pd_factory = primitiveFactory "arrayList",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_StringMap = PrimitiveDetails {
  pd_type = \_ targs -> do
    hashMapI <- addImport "java.util.HashMap"
    return (withTypeArgs hashMapI ("String":targs)),
  pd_default = Nothing,
  pd_genLiteral = \(JSON.String s) -> "???", -- never called
  pd_mutable = True,
  pd_factory = primitiveFactory "stringMap",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_String = PrimitiveDetails {
  pd_type = \_ _ -> return "String",
  pd_default = Just "\"\"",
  pd_genLiteral = \(JSON.String s) -> T.pack (show s),
  pd_mutable= False,
  pd_factory = primitiveFactory "STRING",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_Sink = PrimitiveDetails {
  pd_type = \_ args -> return (withTypeArgs "Sink" args),
  pd_default = Just "new Sink()",
  pd_genLiteral = \_ -> "????", -- never called
  pd_mutable = True,
  pd_factory = primitiveFactory "SINK",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }

primitiveFactory :: T.Text -> CState T.Text
primitiveFactory name = do
  rtpackage <- getRuntimePackage
  factories <- addImport (javaClass rtpackage "Factories")
  return (template "$1.$2" [factories,name])

data FieldDetails = FieldDetails {
  fd_field :: Field CResolvedType,
  fd_memberVarName :: Ident,
  fd_varName :: Ident,
  fd_accessorName :: Ident,
  fd_mutatorName :: Ident,
  fd_unionCtorName :: Ident,
  fd_serializedName :: Ident,
  fd_accessExpr :: Ident,
  fd_typeExprStr :: T.Text,
  fd_boxedTypeExprStr :: T.Text,
  fd_factoryExprStr :: T.Text,
  fd_defValue :: T.Text,
  fd_copy :: T.Text -> T.Text,
  fd_hashcode :: T.Text -> T.Text,
  fd_equals :: T.Text -> T.Text -> T.Text
}

unboxedPrimitive :: T.Text -> T.Text -> TypeBoxed -> [T.Text] -> CState T.Text
unboxedPrimitive _ boxed TypeBoxed _ = return boxed
unboxedPrimitive unboxed _ TypeUnboxed _ = return unboxed

unboxedField fd = fd_typeExprStr fd /= fd_boxedTypeExprStr fd

needsNullCheck fd = not (unboxedField fd || fd_typeExprStr fd == "Void")

immutableType te = case te of
  (TypeExpr (RT_Primitive pt) _) -> not (pd_mutable (genPrimitiveDetails pt))
  _-> False

genFieldDetails :: Field CResolvedType -> CState FieldDetails
genFieldDetails f = do
  let te = f_type f
  typeExprStr <- genTypeExprB TypeUnboxed te
  boxedTypeExprStr <- genTypeExprB TypeBoxed te
  factoryExprStr <- genFactoryExpr te
  litv <- case f_default f of
    (Just v) -> case literalForTypeExpr te v of
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
      varName = unreserveWord (f_name f)
      memberVarName =
        if hungarian
        then "m" <> javaCapsFieldName f
        else unreserveWord (f_name f)
      accessorName = template "get$1" [javaCapsFieldName f]
      mutatorName = template "set$1" [javaCapsFieldName f]
      accessExpr =
        if cgp_publicMembers cgp
        then memberVarName
        else accessorName <> "()"
      unionCtorName = unreserveWord (f_name f)
      serializedName = f_serializedName f
      hashCode =
        if isVoidType te
          then \_ -> "0"
          else case te of
            (TypeExpr (RT_Primitive pt) []) -> \v -> pd_hashfn (genPrimitiveDetails pt) v
            _ -> \v -> template "$1.hashCode()" [v]
      equals =
        if isVoidType te
          then \_ _ -> "true"
          else if typeExprStr /= boxedTypeExprStr  -- ie is unboxed
               then \v o -> template "$1 == $2" [v,o]
               else \v o -> template "$1.equals($2)" [v,o]
                     

  return FieldDetails {
    fd_field=f,
    fd_varName=varName,
    fd_memberVarName=memberVarName,
    fd_accessExpr=accessExpr,
    fd_accessorName=accessorName,
    fd_mutatorName=mutatorName,
    fd_unionCtorName=unionCtorName,
    fd_serializedName=serializedName,
    fd_typeExprStr=typeExprStr,
    fd_boxedTypeExprStr=boxedTypeExprStr,
    fd_factoryExprStr=factoryExprStr,
    fd_defValue=defValue,
    fd_copy=copy,
    fd_hashcode=hashCode,
    fd_equals=equals
    }

-- Inside the union implementation we need to be able to cast
-- from Object to the type of the branch. For a simple enough type
-- (T) v is enough. When generics are involved we need to call
-- a helper function to suppress the warnings.
    
needsSuppressedCheckInCast :: TypeExpr CResolvedType -> Bool
needsSuppressedCheckInCast (TypeExpr (RT_Param _) []) = True
needsSuppressedCheckInCast (TypeExpr _ []) = False
needsSuppressedCheckInCast _ = True


generateDocString :: Annotations a -> Code
generateDocString annotations = case Map.lookup (ScopedName (ModuleName []) "Doc") annotations of
   (Just (_,JSON.String text)) -> docStringComment text
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
genLiteralText (LDefault te@(TypeExpr (RT_Param i) [])) = do
  typeExpr <- genTypeExpr te
  return (template "$1.create()" [factoryTypeArg i])
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
  rtpackage <- getRuntimePackage
  factories <- addImport (javaClass rtpackage "Factories")
  return (template "$1.arrayList($2)" [factories,commaSep lits])
genLiteralText (LStringMap _ kvPairs) = do
  kvlits <- for (Map.toList kvPairs) $ \(k,v) -> do
    litv <- genLiteralText v
    return (template "\"$1\", $2" [k,litv])
  rtpackage <- getRuntimePackage
  factories <- addImport (javaClass rtpackage "Factories")
  return (template "$1.stringMap($2)" [factories,commaSep kvlits])
genLiteralText (LPrimitive pt jv) = do
  return (pd_genLiteral (genPrimitiveDetails pt) jv)

commaSep :: [T.Text] -> T.Text
commaSep = T.intercalate ", "

sortedImports :: [JavaClass] -> [JavaClass]
sortedImports imports = map snd (sort [(classify i,i) | i <- imports])
  where
    classify (JavaClass ("java":_)) = 2
    classify (JavaClass ("android":_)) = 1
    classify _ = 0

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

classFromScopedName :: ScopedName -> JavaClass
classFromScopedName scopedName = javaClass (JavaPackage (unModuleName (sn_moduleName scopedName))) (sn_name scopedName)

discriminatorName :: FieldDetails -> Ident
discriminatorName = T.toUpper . unreserveWord . f_name . fd_field

leadSpace :: T.Text -> T.Text
leadSpace "" = ""
leadSpace t = " " <> t

-- | If a javaclass refers to the default standard library location, update
--   it reference the configured standard library location
fixStdRef :: JavaClass -> CState JavaClass
fixStdRef (JavaClass ("org":"adl":"runtime":[ident])) = do
  rtpackage <- (cgp_runtimePackage . cf_codeProfile) <$> get
  return (javaClass rtpackage ident)
fixStdRef jclass = return jclass

