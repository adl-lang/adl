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
  ct_helpers :: ScopedName,
  ct_generateType :: Bool
  } deriving (Show)

-- A variant of the AST that carries custom type
-- information.

type CResolvedType = ResolvedTypeT (Maybe CustomType)
type CTypeExpr = TypeExpr (CResolvedType)
type CModule = Module (Maybe CustomType) CResolvedType
type CDecl = Decl (Maybe CustomType) CResolvedType

getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType scopedName decl = case Map.lookup javaCustomType (d_annotations decl) of
  Nothing -> Nothing
  Just (_,json) -> Just (convertCustomType json)
  where
    convertCustomType :: JSON.Value -> CustomType
    convertCustomType jv = case aFromJSON (jsonSerialiser (JSONFlags True)) jv of
      Nothing -> error "BUG: failed to parse java custom type"
      (Just jct) -> CustomType
        { ct_scopedName = parseScopedName (JC.javaCustomType_javaname jct)
        , ct_helpers = parseScopedName (JC.javaCustomType_helpers jct)
        , ct_generateType = (JC.javaCustomType_generateType jct)
        }
    
    javaCustomType = ScopedName (ModuleName ["adlc","config","java"]) "JavaCustomType"
    
    parseScopedName :: T.Text -> ScopedName
    parseScopedName t = case P.parse P.scopedName "" t of
      (Right sn) -> sn
      _ -> error ("failed to parse scoped name '" <> T.unpack t <> "' in java custom type for " <> T.unpack (formatText scopedName))


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

-- | Generate the java code expressing a type.
genTypeExpr :: TypeExpr CResolvedType -> CState T.Text
genTypeExpr te = genTypeExprB TypeUnboxed te

genTypeExprB :: TypeBoxed ->  TypeExpr CResolvedType -> CState T.Text
genTypeExprB boxed (TypeExpr rt params) = do
  rtParamsStr <- mapM (genTypeExprB TypeBoxed) params
  td_type (getTypeDetails rt) boxed rtParamsStr

-- | Generate an expression referencing or constructing a
-- factory for a type.
genFactoryExpr :: TypeExpr CResolvedType -> CState T.Text
genFactoryExpr (TypeExpr rt params) = do
  fparams <- mapM genFactoryExpr params
  td_factory (getTypeDetails rt) fparams

-- | Generate an expression to construct a literal value.
genLiteralText :: Literal CTypeExpr -> CState T.Text
genLiteralText lit@(LDefault (TypeExpr rt tes))  = td_genLiteralText (getTypeDetails rt) lit
genLiteralText lit@(LCtor (TypeExpr rt _) _)     = td_genLiteralText (getTypeDetails rt) lit
genLiteralText lit@(LUnion (TypeExpr rt _) _ _ ) = td_genLiteralText (getTypeDetails rt) lit
genLiteralText lit@(LPrimitive pt _)             = td_genLiteralText (getTypeDetails (RT_Primitive pt)) lit
genLiteralText lit@(LVector _ ls)                = td_genLiteralText (getTypeDetails (RT_Primitive P_Vector)) lit
genLiteralText lit@(LStringMap _ kvPairs)        = td_genLiteralText (getTypeDetails (RT_Primitive P_StringMap)) lit

-- | The key functions needed to plug a type into the
-- code generator
data TypeDetails = TypeDetails {

  -- | Generate the textual representation of the type,
  -- given the representation of the type arguments.
  td_type :: TypeBoxed -> [T.Text] -> CState T.Text,

  -- | Generate the text for a literal of the type.
  -- Implementations can be partial they only need to
  -- handle the relevant Literal cases.
  td_genLiteralText :: Literal CTypeExpr -> CState T.Text,

  -- | True if the type is mutable. In particular classes
  -- that are not mutable can be copied with "=".
  td_mutable :: Bool,

  -- | Construct an expression for a Factory instance
  -- for this type.
  td_factory :: [T.Text] -> CState T.Text,

  -- | Return an expression to hash a value of this type.
  td_hashfn :: T.Text -> T.Text
}

-- | Get the TypeDetails record for any resolved type.
getTypeDetails :: CResolvedType -> TypeDetails

-- a type defined through a regular ADL declaration
getTypeDetails rt@(RT_Named (scopedName,Decl{d_customType=Nothing})) = TypeDetails
  { td_type =  \_ typeArgs -> do
      ts <- genScopedName scopedName
      return (withTypeArgs ts typeArgs)
  , td_genLiteralText = genLiteralText'
  , td_mutable = True
  , td_factory = \params -> do
      sn <- genScopedName scopedName
      case params of
        [] -> return (sn <> ".FACTORY")
        _  -> return (withParams (sn <> ".factory") params)
  , td_hashfn = \v -> template "$1.hashCode()" [v]
  }
  where
    genLiteralText' (LDefault te@(TypeExpr _ [])) = do
      typeExpr <- genTypeExpr te
      return (template "new $1()" [typeExpr])
    genLiteralText' (LDefault te) = do
      factoryExpr <- genFactoryExpr te
      return (template "$1.create()" [factoryExpr])
    genLiteralText' (LCtor te ls) = do
      typeExpr <- genTypeExpr te
      lits <- mapM genLiteralText ls
      return (template "new $1($2)" [typeExpr, T.intercalate ", " lits])
    genLiteralText' (LUnion te ctor l) = do
      typeExpr <- genTypeExpr te
      lit <- genLiteralText l
      return (template "$1.$2($3)" [typeExpr, ctor, lit ])

-- a custom type
getTypeDetails rt@(RT_Named (_,Decl{d_customType=Just customType})) = TypeDetails
  { td_type = \_ typeArgs -> do
      ts <- addImport (classFromScopedName (ct_scopedName customType))
      return (withTypeArgs ts typeArgs)
  , td_genLiteralText = genLiteralText'
  , td_mutable = True
  , td_factory = \params -> do
      helpers <- getHelpers customType
      case params of
        [] -> return (helpers <> ".FACTORY")
        _  -> return (withParams (helpers <> ".factory") params)
  , td_hashfn = \v -> template "$1.hashCode()" [v]
  }
  where
    genLiteralText' (LDefault te) = do
      factoryExpr <- genFactoryExpr te
      return (template "$1.create()" [factoryExpr])
    genLiteralText' (LCtor te ls) = do
      idHelpers <- getHelpers customType
      lits <- mapM genLiteralText ls
      return (template "$1.create($2)" [idHelpers, T.intercalate ", " lits])
    genLiteralText' (LUnion te ctor l) = do
      idHelpers <- getHelpers customType
      lit <- genLiteralText l
      return (template "$1.$2($3)" [idHelpers, ctor, lit ])

-- a type variable
getTypeDetails (RT_Param typeVar) = TypeDetails
  { td_type = \_ _ -> do
      return (unreserveWord typeVar)
  , td_genLiteralText = genLiteralText'
  , td_mutable = True
  , td_factory = \params -> return (withParams (factoryTypeArg typeVar) params)
  , td_hashfn = \v -> template "$1.hashCode()" [v]
  }
  where
    genLiteralText' (LDefault te) = do
      typeExpr <- genTypeExpr te
      return (template "$1.create()" [factoryTypeArg typeVar])

-- each primitive
getTypeDetails (RT_Primitive P_Void) = TypeDetails
  { td_type = \_ _ -> return "Void"
  , td_genLiteralText = \_ -> return "null"
  , td_mutable = False
  , td_factory = primitiveFactory "VOID"
  , td_hashfn = \_ -> "0"
  }

getTypeDetails (RT_Primitive P_Bool) = TypeDetails
  { td_type = unboxedPrimitive "boolean" "Boolean"
  , td_genLiteralText = genLiteralText'
  , td_mutable = False
  , td_factory = primitiveFactory "BOOLEAN"
  , td_hashfn = \from -> template "($1 ? 0 : 1)" [from]
  }
  where
    genLiteralText' (LDefault _ ) = return "false"
    genLiteralText' (LPrimitive _ (JSON.Bool False)) = return "false"
    genLiteralText' (LPrimitive _ (JSON.Bool True)) = return "true"

getTypeDetails (RT_Primitive P_Int8) = TypeDetails
  { td_type = unboxedPrimitive "byte" "Byte"
  , td_genLiteralText = genLiteralText'
  , td_mutable = False
  , td_factory = primitiveFactory "BYTE"
  , td_hashfn = \from -> template "(int) $1" [from]
  }
  where
    genLiteralText' (LDefault _ ) = return "(byte)0"
    genLiteralText' (LPrimitive _ (JSON.Number n)) = return ("(byte)" <> litNumber n)

getTypeDetails (RT_Primitive P_Int16) = TypeDetails
  { td_type = unboxedPrimitive "short" "Short"
  , td_genLiteralText = genLiteralText'
  , td_mutable = False
  , td_factory = primitiveFactory "SHORT"
  , td_hashfn = \from -> template "(int) $1" [from]
  }
  where
    genLiteralText' (LDefault _ ) = return "(short)0"
    genLiteralText' (LPrimitive _ (JSON.Number n)) = return ("(short)" <> litNumber n)

getTypeDetails (RT_Primitive P_Int32) = TypeDetails
  { td_type = unboxedPrimitive "int" "Integer"
  , td_genLiteralText = genLiteralText'
  , td_mutable = False
  , td_factory = primitiveFactory "INTEGER"
  , td_hashfn = id
  }
  where
    genLiteralText' (LDefault _ ) = return "0"
    genLiteralText' (LPrimitive _ (JSON.Number n)) = return (litNumber n)

getTypeDetails (RT_Primitive P_Int64) = TypeDetails
  { td_type = unboxedPrimitive "long" "Long"
  , td_genLiteralText = genLiteralText'
  , td_mutable = False
  , td_factory = primitiveFactory "LONG"
  , td_hashfn = \from -> template "(int) ($1 ^ ($1 >>> 32))" [from]
  }
  where
    genLiteralText' (LDefault _ ) = return "0L"
    genLiteralText' (LPrimitive _ (JSON.Number n)) = return (litNumber n <> "L")

getTypeDetails (RT_Primitive P_Float) = TypeDetails
  { td_type = unboxedPrimitive "float" "Float"
  , td_genLiteralText = genLiteralText'
  , td_mutable = False
  , td_factory = primitiveFactory "FLOAT"
  , td_hashfn = \from -> template "Float.valueOf($1).hashCode()" [from]
  }
  where
    genLiteralText' (LDefault _ ) = return "0.0F"
    genLiteralText' (LPrimitive _ (JSON.Number n)) = return (litNumber n <> "F")

getTypeDetails (RT_Primitive P_Double) = TypeDetails
  { td_type = unboxedPrimitive "double" "Double"
  , td_genLiteralText = genLiteralText'
  , td_mutable = False
  , td_factory = primitiveFactory "DOUBLE"
  , td_hashfn = \from -> template "Double.valueOf($1).hashCode()" [from]
  }
  where
    genLiteralText' (LDefault _ ) = return "0.0"
    genLiteralText' (LPrimitive _ (JSON.Number n)) = return (litNumber n)

getTypeDetails (RT_Primitive P_Word8) = getTypeDetails (RT_Primitive P_Int8)
getTypeDetails (RT_Primitive P_Word16) = getTypeDetails (RT_Primitive P_Int16)
getTypeDetails (RT_Primitive P_Word32) = getTypeDetails (RT_Primitive P_Int32)
getTypeDetails (RT_Primitive P_Word64) = getTypeDetails (RT_Primitive P_Int64)

getTypeDetails (RT_Primitive P_ByteVector) = TypeDetails
  { td_type = \_ _ -> getType
  , td_genLiteralText = genLiteralText'
  , td_mutable = True
  , td_factory = primitiveFactory "BYTE_ARRAY"
  , td_hashfn = \from -> template "$1.hashCode()" [from]
  }
  where
    getType = do
      rtpackage <- getRuntimePackage
      addImport (javaClass rtpackage "ByteArray")

    genLiteralText' (LDefault _ ) = do
      iByteArray <- getType
      return (template "new $1()" [iByteArray])
    genLiteralText' (LPrimitive _ (JSON.String s)) = do
      iByteArray <- getType
      return (template "new $1($2.getBytes())" [iByteArray,T.pack (show (decode s))])

    decode s = case B64.decode (T.encodeUtf8 s) of
      (Left _) -> "???"
      (Right s) -> s

getTypeDetails (RT_Primitive P_Vector) = TypeDetails
  { td_type = \_ args -> do
       arrayListI <- addImport "java.util.ArrayList"
       return (withTypeArgs arrayListI args)
  , td_genLiteralText = genLiteralText'
  , td_mutable = True
  , td_factory = primitiveFactory "arrayList"
  , td_hashfn = \from -> template "$1.hashCode()" [from]
  }
  where
    genLiteralText' (LDefault te) = do
      typeExpr <- genTypeExpr te
      return (template "new $1()" [typeExpr])
    genLiteralText' (LVector _ ls) = do
      lits <- mapM genLiteralText ls
      rtpackage <- getRuntimePackage
      factories <- addImport (javaClass rtpackage "Factories")
      return (template "$1.arrayList($2)" [factories,commaSep lits])
      
getTypeDetails (RT_Primitive P_StringMap) = TypeDetails
  { td_type = \_ args -> do
       hashMapI <- addImport "java.util.HashMap"
       return (withTypeArgs hashMapI ("String":args))
  , td_genLiteralText = genLiteralText'
  , td_mutable = True
  , td_factory = primitiveFactory "stringMap"
  , td_hashfn = \from -> template "$1.hashCode()" [from]
  }
  where
    genLiteralText' (LDefault te) = do
      typeExpr <- genTypeExpr te
      return (template "new $1()" [typeExpr])
    genLiteralText' (LStringMap _ kvPairs) = do
      kvlits <- for (Map.toList kvPairs) $ \(k,v) -> do
        litv <- genLiteralText v
        return (template "\"$1\", $2" [k,litv])
      rtpackage <- getRuntimePackage
      factories <- addImport (javaClass rtpackage "Factories")
      return (template "$1.stringMap($2)" [factories,commaSep kvlits])

getTypeDetails (RT_Primitive P_String) = TypeDetails
  { td_type = \_ _ -> return "String"
  , td_genLiteralText = genLiteralText'
  , td_mutable = False
  , td_factory = primitiveFactory "STRING"
  , td_hashfn = \from -> template "$1.hashCode()" [from]
  }
  where
    genLiteralText' (LDefault _ ) = return "\"\"";
    genLiteralText' (LPrimitive _ (JSON.String s)) = return (T.pack (show s))

getTypeDetails (RT_Primitive P_Sink) = TypeDetails
  { td_type = \_ args -> getType
  , td_genLiteralText = error "BUG: td_genLiteralText shouldn't be called on a Sink"
  , td_mutable = True
  , td_factory = primitiveFactory "SINK"
  , td_hashfn = \from -> template "$1.hashCode()" [from]
  }
  where
    getType = do
      rtpackage <- getRuntimePackage
      addImport (javaClass rtpackage "Sink")

primitiveFactory :: Ident -> [T.Text] -> CState T.Text
primitiveFactory name params =  do
  rtpackage <- getRuntimePackage
  factories <- addImport (javaClass rtpackage "Factories")
  return (withParams (factories <> "." <> name) params)

withTypeArgs :: T.Text -> [T.Text] -> T.Text
withTypeArgs ts [] = ts
withTypeArgs ts tsargs = template "$1<$2>" [ts,T.intercalate ", " tsargs]

withParams :: T.Text -> [T.Text] -> T.Text
withParams v [] = v
withParams f args = template "$1($2)" [f,T.intercalate ", " args]

genScopedName :: ScopedName -> CState T.Text
genScopedName scopedName0 = do
  cf <- get
  let scopedName =
        case sn_moduleName scopedName0 of
          (ModuleName []) -> ScopedName (cf_module cf) (sn_name scopedName0)
          _ -> scopedName0
  addImport (javaClass (cf_javaPackageFn cf (sn_moduleName scopedName)) (sn_name scopedName))


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

immutableType :: TypeExpr CResolvedType -> Bool
immutableType te = let (TypeExpr rt _) = te in not (td_mutable (getTypeDetails rt))

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
          then const "0"
          else let (TypeExpr rt _) = te in td_hashfn (getTypeDetails rt)
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
generateDocString annotations = case Map.lookup (ScopedName (ModuleName ["sys","annotations"]) "Doc") annotations of
   (Just (_,JSON.String text)) -> docStringComment text
   _ -> mempty

docStringComment :: T.Text -> Code
docStringComment text = cline "/**" <> mconcat [cline (" * " <> line) | line <- T.lines text] <> cline " */"

multiLineComment :: T.Text -> Code
multiLineComment text = cline "/*" <> mconcat [cline (" * " <> line) | line <- T.lines text] <> cline " */"

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

