{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Java(
  generate,
  JavaFlags(..),
  CodeGenProfile(..),
  defaultCodeGenProfile,
  ) where

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import qualified Data.Aeson as JSON
import Data.Char(toUpper)
import Data.Maybe(fromMaybe,isJust)
import Data.Foldable(for_)
import Data.List(intersperse,replicate,sort)
import Data.Monoid
import Data.Traversable(for)

import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Backends.Literals
import ADL.Utils.Format

data JavaFlags = JavaFlags {
  -- directories where we look for ADL files
  jf_searchPath :: [FilePath],

  -- The java package under which we hang the generated ADL
  jf_package :: T.Text,
  jf_fileWriter :: FilePath -> LBS.ByteString -> IO (),

  jf_codeGenProfile :: CodeGenProfile
  }

newtype JavaPackage = JavaPackage {
  unJavaPackage :: [Ident]
}

genJavaPackage :: JavaPackage -> T.Text
genJavaPackage package = T.intercalate "." (map unreserveWord (unJavaPackage package))

data CodeGenProfile = CodeGenProfile {
  cgp_header :: T.Text,
  cgp_maxLineLength :: Int,
  cgp_mutable :: Bool,
  cgp_hungarianNaming :: Bool,
  cgp_publicMembers :: Bool,
  cgp_genericFactories :: Bool,
  cgp_parcelable :: Bool,
  cgp_runtimePackage :: T.Text
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
   cf_package :: JavaPackage,
   cf_imports :: Set.Set T.Text,
   cf_implements :: Set.Set T.Text,
   cf_docString :: Code,
   cf_decl :: T.Text,
   cf_fields :: [Code],
   cf_methods :: [Code]
}

classFile :: CodeGenProfile -> ModuleName -> JavaPackage -> T.Text -> ClassFile
classFile codeProfile mname javapackage decl = ClassFile codeProfile mname javapackage Set.empty Set.empty mempty decl [] []

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
  mconcat [ctemplate "import $1;" [imp] | imp <- sortedImports (cf_imports content)]
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
    header = cgp_header (cf_codeProfile content)
    decl | Set.null (cf_implements content) = (template "$1" [cf_decl content])
         | otherwise = (template "$1 implements $2" [cf_decl content,commaSep (Set.toList (cf_implements content))])
type CState a = State ClassFile a

instance MGen (State ClassFile) where
  getPrimitiveType pt = let pd = genPrimitiveDetails pt in
    case pd_unboxed pd of
      Nothing -> pd_type pd
      (Just t) -> t
  getPrimitiveDefault pt = return (pd_default (genPrimitiveDetails pt))
  getPrimitiveLiteral pt jv = return (pd_genLiteral (genPrimitiveDetails pt) jv)
  getTypeExpr _ te = genTypeExpr te
  getTypeExprB _ _ te = genTypeExpr te
  getUnionConstructorName d f = return (unreserveWord (f_name f))

addField :: Code -> CState ()
addField decl = modify (\cf->cf{cf_fields=decl:cf_fields cf})

addMethod :: Code -> CState ()
addMethod method = modify (\cf->cf{cf_methods=method:cf_methods cf})

addImport :: T.Text -> CState ()
addImport imp = modify (\cf->cf{cf_imports=Set.insert imp (cf_imports cf)})

addImplements :: T.Text -> CState ()
addImplements imp = modify (\cf->cf{cf_implements=Set.insert imp (cf_implements cf)})

setDocString :: Code -> CState ()
setDocString code = modify (\cf->cf{cf_docString=code})

getRuntimePackage :: CState T.Text
getRuntimePackage = (cgp_runtimePackage . cf_codeProfile) <$> get

genTypeExpr :: TypeExpr ResolvedType -> CState T.Text
genTypeExpr te = genTypeExprB False te

genTypeExprB :: Bool ->  TypeExpr ResolvedType -> CState T.Text
genTypeExprB boxed (TypeExpr rt []) = genResolvedType boxed rt
genTypeExprB boxed (TypeExpr rt params) = do
  rtStr <- genResolvedType boxed rt
  rtParamsStr <- mapM (genTypeExprB True) params
  return (template "$1<$2>" [rtStr,T.intercalate ", " rtParamsStr])

genResolvedType :: Bool -> ResolvedType -> CState T.Text
genResolvedType _ (RT_Named (scopedName,_)) = genScopedName scopedName
genResolvedType _(RT_Param ident) = return (unreserveWord ident)
genResolvedType False (RT_Primitive pt) = let pd = genPrimitiveDetails pt in fromMaybe (pd_type pd) (pd_unboxed pd)
genResolvedType True (RT_Primitive pt) = pd_type (genPrimitiveDetails pt)

genScopedName :: ScopedName -> CState T.Text
genScopedName scopedName = do
  currentModuleName <- fmap cf_module get
  let mname = sn_moduleName scopedName
  if mname  == currentModuleName
    then return (sn_name scopedName)
    else return (T.intercalate "." (map unreserveWord (unModuleName mname <> [sn_name scopedName])))

genFactoryExpr :: TypeExpr ResolvedType -> CState T.Text
genFactoryExpr (TypeExpr rt params) = do
  fparams <- mapM genFactoryExpr params
  fe <- case rt of
    (RT_Named (scopedName,_)) -> do
      fe <- genScopedName scopedName
      case params of
        [] -> return (template "$1.FACTORY" [fe])
        _ -> return (template "$1.factory" [fe])
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
    addImport (rtpackage <> ".ByteArray")
    return "ByteArray",
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
  pd_type = return "java.util.ArrayList",
  pd_default = Just "new java.util.ArrayList()",
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
  addImport (rtpackage <> ".Factories") >> return (template "Factories.$1" [name])

data FieldDetails = FieldDetails {
  fd_field :: Field ResolvedType,
  fd_fieldName :: Ident,
  fd_typeExprStr :: T.Text,
  fd_boxedTypeExprStr :: T.Text,
  fd_factoryExprStr :: T.Text,
  fd_defValue :: Literal,
  fd_copy :: T.Text -> T.Text
}

unboxedField fd = case (f_type (fd_field fd)) of
  (TypeExpr (RT_Primitive pt) []) -> isJust (pd_unboxed (genPrimitiveDetails pt))
  _ -> False

needsNullCheck fd = not (unboxedField fd || fd_typeExprStr fd == "Void")

immutableType te = case te of
  (TypeExpr (RT_Primitive pt) _) -> not (pd_mutable (genPrimitiveDetails pt))
  _-> False

genFieldDetails :: Field ResolvedType -> CState FieldDetails
genFieldDetails f = do
  let te = f_type f
  typeExprStr <- genTypeExprB False te
  boxedTypeExprStr <- genTypeExprB True te
  factoryExprStr <- genFactoryExpr te
  litv <- case f_default f of
    (Just v) -> mkLiteral te v
    Nothing -> mkDefaultLiteral te
  cgp <- cf_codeProfile <$> get

  let copy from =
        if immutableType te
          then from
          else template "$1.create($2)" [factoryExprStr,from]
      hungarian = cgp_hungarianNaming cgp && not (cgp_publicMembers cgp)
      fieldName = if hungarian then "m" <> javaCapsFieldName f else unreserveWord (f_name f)

  return (FieldDetails f fieldName typeExprStr boxedTypeExprStr factoryExprStr litv copy)



generateModule :: (ModuleName -> JavaPackage) ->
                  (ScopedName -> FilePath) ->
                  (ScopedName -> CodeGenProfile) ->
                  (FilePath -> LBS.ByteString -> IO ()) ->
                  Module ResolvedType ->
                  EIO T.Text ()
generateModule mPackage mFile mCodeGetProfile fileWriter m0 = do
  let m = removeModuleTypedefs (expandModuleTypedefs m0)
      decls = Map.elems (m_decls m)
  for_ decls $ \decl -> do
    let moduleName = m_name m
        javaPackage = mPackage moduleName
        codeProfile = mCodeGetProfile (ScopedName moduleName (d_name decl))
        maxLineLength = cgp_maxLineLength codeProfile
        file = mFile (ScopedName moduleName (unreserveWord (d_name decl)))
    case d_type decl of
      (Decl_Struct s) -> writeClassFile file maxLineLength (generateStruct codeProfile moduleName javaPackage decl s)
      (Decl_Union u)  -> writeClassFile file maxLineLength (generateUnion codeProfile moduleName javaPackage decl u)
      (Decl_Newtype n) -> writeClassFile file maxLineLength (generateNewtype codeProfile moduleName javaPackage decl n)
      (Decl_Typedef _) -> eioError "BUG: typedefs should have been eliminated"
  where
    writeClassFile :: FilePath -> Int -> ClassFile -> EIO a ()
    writeClassFile path maxLineLength cfile = do
      let lines = codeText maxLineLength (classFileCode cfile)
      liftIO $ fileWriter path (LBS.fromStrict (T.encodeUtf8 (T.intercalate "\n" lines <> "\n")))
      

generateStruct :: CodeGenProfile -> ModuleName -> JavaPackage -> Decl ResolvedType -> Struct ResolvedType -> ClassFile
generateStruct codeProfile moduleName javaPackage decl struct =  execState gen state0
  where
    state0 = classFile codeProfile moduleName javaPackage classDecl
    isEmpty = null (s_fields struct)
    className = unreserveWord (d_name decl)
    classDecl = "public class " <> className <> typeArgs
    typeArgs = case s_typeParams struct of
      [] -> ""
      args -> "<" <> commaSep (map unreserveWord args) <> ">"
    gen = do
      setDocString (generateDocString (d_annotations decl))
      fieldDetails <- mapM genFieldDetails (s_fields struct)

      -- Fields
      for_ fieldDetails $ \fd -> do
        let modifiers =
             (if cgp_publicMembers codeProfile then ["public"] else ["private"])
             <>
             (if cgp_mutable codeProfile then [] else ["final"])
        addField (ctemplate "$1 $2 $3;" [T.intercalate " " modifiers,fd_typeExprStr fd,fd_fieldName fd])

      -- Constructors
      let ctorArgs =  T.intercalate ", " [fd_typeExprStr fd <> " " <> fd_fieldName fd | fd <- fieldDetails]
          isGeneric = length (s_typeParams struct) > 0
          
          ctor1 =
            cblock (template "public $1($2)" [className,ctorArgs]) (
              clineN [
                if needsNullCheck fd
                  then template "this.$1 = java.util.Objects.requireNonNull($1);" [fd_fieldName fd]
                  else template "this.$1 = $1;" [fd_fieldName fd]
                | fd <- fieldDetails]
            )

          ctor2 =
            cblock (template "public $1()" [className]) (
              clineN [template "this.$1 = $2;" [fd_fieldName fd,literalValue (fd_defValue fd)] | fd <- fieldDetails]
            )

          ctor3 =
            cblock (template "public $1($2 other)" [className, className <> typeArgs]) (
              mconcat [ let n = fd_fieldName fd in ctemplate "this.$1 = $2;" [n,fd_copy fd ("other." <>n)]
                      | fd <- fieldDetails ]
            )

      addMethod (cline "/* Constructors */")

      addMethod ctor1
      when (not isGeneric && not isEmpty) (addMethod ctor2)
      when (not isGeneric) (addMethod ctor3)

      -- Getters/Setters
      when (not isEmpty) (addMethod (cline "/* Accessors and mutators */"))
      
      when (not (cgp_publicMembers codeProfile)) $ do
        for_ fieldDetails $ \fd -> do
          let fieldName = fd_fieldName fd
              capsFieldName = javaCapsFieldName (fd_field fd)
              typeExprStr = fd_typeExprStr fd
              getter =
                cblock (template "public $1 get$2()" [typeExprStr,capsFieldName]) (
                  ctemplate "return $1;" [fieldName]
                )
              setter =
                cblock (template "public void set$1($2 new$1)" [capsFieldName,typeExprStr]) (
                  if needsNullCheck fd
                     then ctemplate "$1 = java.util.Objects.requireNonNull(new$2);" [fieldName,capsFieldName]
                     else ctemplate "$1 = new$2;" [fieldName,capsFieldName]
                )
          addMethod getter
          when (cgp_mutable codeProfile) (addMethod setter)

      -- equals and hashcode
      addMethod (cline "/* Object level helpers */")

      let equals = coverride "public boolean equals(Object other0)" (
            cblock (template "if (!(other0 instanceof $1))"  [className]) (
              cline "return false;"
              )
            <>
            ctemplate "$1 other = ($1) other0;" [className]
            <>
            cline "return"
            <>
            let terminators = replicate (length fieldDetails-1) " &&" <> [";"]
                tests = [ctemplate (if unboxedField fd then "$1 == other.$1$2" else "$1.equals(other.$1)$2")
                                   [fd_fieldName fd,term]
                        | (fd,term) <- zip fieldDetails terminators]
            in  indent (mconcat tests)
            )
          equalsEmpty = coverride "public boolean equals(Object other)" (
            ctemplate "return other instanceof $1;" [className]
            )
      addMethod (if isEmpty then equalsEmpty else equals)

      addMethod $ coverride "public int hashCode()" (
        cline "int result = 1;"
        <>
        let hashfn fd from = case (f_type (fd_field fd)) of
              (TypeExpr (RT_Primitive pt) []) -> pd_hashfn (genPrimitiveDetails pt) from
              _ -> template "$1.hashCode()" [from]
        in mconcat [ctemplate "result = result * 37 + $1;" [hashfn fd (fd_fieldName fd)] | fd <- fieldDetails]
        <>
        cline "return result;"
        )

      -- factory
      let factory =
            cblock1 (template "public static final Factory<$1> FACTORY = new Factory<$1>()" [className]) (
              cblock (template "public $1 create()" [className]) (
                 ctemplate "return new $1();" [className]
              )
              <>
              cblock (template "public $1 create($1 other)" [className]) (
                 ctemplate "return new $1(other);" [className]
              )
            )

      let factoryg =
            cblock (template "public static $2 Factory<$1$2> factory($3)" [className,typeArgs,factoryArgs]) (
              cblock1 (template "return new Factory<$1$2>()" [className,typeArgs]) (
                mconcat [ctemplate "final Factory<$1> $2 = $3;" [fd_boxedTypeExprStr fd,fd_fieldName fd,fd_factoryExprStr fd] | fd <- fieldDetails, not (immutableType (f_type (fd_field fd)))]
                <>
                cline ""
                <>
                cblock (template "public $1$2 create()" [className,typeArgs]) (
                   ctemplate "return new $1$2($3);" [className,typeArgs,ctor1Args]
                )
                <>
                cline ""
                <>
                cblock (template "public $1$2 create($1$2 other)" [className,typeArgs]) (
                   ctemplate "return new $1$2($3);" [className,typeArgs,ctor2Args]
                )
              )
            )

          factoryArgs = commaSep [template "Factory<$1> $2" [arg,factoryTypeArg arg] | arg <- s_typeParams struct]
          ctor1Args = commaSep [if immutableType (f_type (fd_field fd))
                                then literalValue (fd_defValue fd)
                                else template "$1.create()" [fd_fieldName fd] | fd <-fieldDetails]
          ctor2Args = commaSep [if immutableType (f_type (fd_field fd))
                                then template "other.$1" [fieldAccessExpr codeProfile fd]
                                else template "$1.create(other.$2)" [fd_fieldName fd,fieldAccessExpr codeProfile fd]
                               | fd <- fieldDetails]

      addMethod (cline "/* Factory for construction of generic values */")

      addImport (cgp_runtimePackage codeProfile <> ".Factory")
      addMethod (if isGeneric then factoryg else factory)

      -- Parcelable
      when (cgp_parcelable codeProfile) $ do
        importParcelable
        addImplements "Parcelable"

        addMethod (cline "/* Android Parcelable implementation */")
        
        addMethod $ coverride "public int describeContents()" (
          cline "return 0;"
          )

        writeFields <- for fieldDetails $ \fd -> do
          writeToParcel (f_type (fd_field fd)) "out" (fd_fieldName fd) "flags"

        readFields <- for fieldDetails $ \fd -> do
          readFromParcel (f_type (fd_field fd)) (Just (fd_typeExprStr fd)) (fd_fieldName fd) "in"

        addMethod $ coverride "public void writeToParcel(Parcel out, int flags)" (
          mconcat writeFields
          )

        addMethod $ cblock1 (template "public static final Parcelable.Creator<$1> CREATOR = new Parcelable.Creator<$1>()" [className]) (
          coverride (template "public $1 createFromParcel(Parcel in)" [className]) (
            mconcat readFields
            <>
            ctemplate "return new $1($2);" [className,commaSep [fd_fieldName fd | fd <- fieldDetails]]
            )
          <>
          cline ""
          <>
          coverride (template "public $1[] newArray(int size)" [className]) (
            ctemplate "return new $1[size];" [className]
            )
          )

importParcelable :: CState ()
importParcelable = do
  addImport "android.os.Parcel"
  addImport "android.os.Parcelable"

writeToParcel :: TypeExpr ResolvedType -> Ident -> Ident -> Ident -> CState Code
writeToParcel te to from flags = return $ case te of
  (TypeExpr (RT_Primitive P_Void) _) -> mempty
  (TypeExpr (RT_Primitive P_Bool) _) -> ctemplate "$1.writeByte($2 ? (byte) 1 : (byte) 0);" [to,from]
  (TypeExpr (RT_Primitive P_Int8) _) -> ctemplate "$1.writeByte($2);" [to,from]
  (TypeExpr (RT_Primitive P_Int16) _) -> ctemplate "$1.writeInt($2);" [to,from]
  (TypeExpr (RT_Primitive P_Int32) _) -> ctemplate "$1.writeInt($2);" [to,from]
  (TypeExpr (RT_Primitive P_Int64) _) -> ctemplate "$1.writeLong($2);" [to,from]
  (TypeExpr (RT_Primitive P_Word8) _) -> ctemplate "$1.writeByte($2);" [to,from]
  (TypeExpr (RT_Primitive P_Word16) _) -> ctemplate "$1.writeInt($2);" [to,from]
  (TypeExpr (RT_Primitive P_Word32) _) -> ctemplate "$1.writeInt($2);" [to,from]
  (TypeExpr (RT_Primitive P_Word64) _) -> ctemplate "$1.writeLong($2);" [to,from]
  (TypeExpr (RT_Primitive P_Float) _) -> ctemplate "$1.writeFloat($2);" [to,from]
  (TypeExpr (RT_Primitive P_Double) _) -> ctemplate "$1.writeDouble($2);" [to,from]
  (TypeExpr (RT_Primitive P_ByteVector) _) -> ctemplate "$1.writeByteArray($2.getValue());" [to,from]
  (TypeExpr (RT_Primitive P_String) _) -> ctemplate "$1.writeString($2);" [to,from]
  (TypeExpr (RT_Primitive P_Vector) _) -> ctemplate "$1.writeList($2);" [to,from]
  _ -> ctemplate "$1.writeToParcel($2, $3);" [from,to,flags]
                                 
readFromParcel :: TypeExpr ResolvedType -> Maybe Ident -> Ident -> Ident -> CState Code
readFromParcel te mtotype tovar from = do
  let to = case mtotype of
        Nothing -> tovar
        (Just totype) -> totype <> " " <> tovar
  case te of
    (TypeExpr (RT_Primitive P_Void) _) -> return $ctemplate "$1 = null;" [to]
    (TypeExpr (RT_Primitive P_Bool) _) -> return $ ctemplate "$1 = $2.readByte() != 0;" [to,from]
    (TypeExpr (RT_Primitive P_Int8) _) -> return $ ctemplate "$1 = $2.readByte();" [to,from]
    (TypeExpr (RT_Primitive P_Int16) _) -> return $ ctemplate "$1 = $2.readInt();" [to,from]
    (TypeExpr (RT_Primitive P_Int32) _) -> return $ ctemplate "$1 = $2.readInt();" [to,from]
    (TypeExpr (RT_Primitive P_Int64) _) -> return $ ctemplate "$1 = $2.readLong();" [to,from]
    (TypeExpr (RT_Primitive P_Word8) _) -> return $ ctemplate "$1 = $2.readByte();" [to,from]
    (TypeExpr (RT_Primitive P_Word16) _) -> return $ ctemplate "$1 = $2.readInt();" [to,from]
    (TypeExpr (RT_Primitive P_Word32) _) -> return $ ctemplate "$1 = $2.readInt();" [to,from]
    (TypeExpr (RT_Primitive P_Word64) _) -> return $ ctemplate "$1 = $2.readLong();" [to,from]
    (TypeExpr (RT_Primitive P_Float) _) -> return $ ctemplate "$1 = $2.readFloat();" [to,from]
    (TypeExpr (RT_Primitive P_Double) _) -> return $ ctemplate "$1 = $2.readDouble();" [to,from]
    (TypeExpr (RT_Primitive P_String) _) -> return $ ctemplate "$1 = $2.readString();" [to,from]
    (TypeExpr (RT_Primitive P_ByteVector) _) -> do
      return (
        ctemplate "$1 = new ByteArray($2.createByteArray());" [to,from]
        )
    (TypeExpr (RT_Primitive P_Vector) [te']) ->  do
      typeExprStr <- genTypeExprB True te'
      return (
        ctemplate "$1 = new java.util.ArrayList<$2>();" [to,typeExprStr]
        <>
        ctemplate "$1.readList($2, $3.class.getClassLoader());" [from,tovar,typeExprStr]
        )
    _ -> do
      typeExprStr <- genTypeExprB True te
      return (
        ctemplate "$1 = $2.CREATOR.createFromParcel($3);" [to,typeExprStr,from]
        )

generateNewtype :: CodeGenProfile -> ModuleName -> JavaPackage -> Decl ResolvedType -> Newtype ResolvedType -> ClassFile
generateNewtype codeProfile moduleName javaPackage decl newtype_ =
  -- In java a newtype is just a single valuesed struct
  generateStruct codeProfile moduleName javaPackage decl struct
  where
    struct = Struct {
      s_typeParams = n_typeParams newtype_,
      s_fields =
        [ Field {
           f_name = "value",
           f_type = n_typeExpr newtype_,
           f_default = n_default newtype_,
           f_annotations = mempty
           }
        ]
      }

generateUnion :: CodeGenProfile -> ModuleName -> JavaPackage -> Decl ResolvedType -> Union ResolvedType -> ClassFile
generateUnion codeProfile moduleName javaPackage decl union =  execState gen state0
  where
    state0 = classFile codeProfile moduleName javaPackage classDecl
    className = unreserveWord (d_name decl)
    classDecl = "public class " <> className <> typeArgs
    isGeneric = length (u_typeParams union) > 0
    discVar = if cgp_hungarianNaming codeProfile then "mDisc" else "disc"
    valueVar = if cgp_hungarianNaming codeProfile then "mValue" else "value"
    typeArgs = case u_typeParams union of
      [] -> ""
      args -> "<" <> commaSep (map unreserveWord args) <> ">"
    gen = do
      setDocString (generateDocString (d_annotations decl))
      fieldDetails <- mapM genFieldDetails (u_fields union)
      fieldDetail0 <- case fieldDetails of
        [] -> error "BUG: unions with no fields are illegal"
        (fd:_) -> return fd

      -- Fields
      let modifiers = T.intercalate " " (["private"] <> if cgp_mutable codeProfile then [] else ["final"])
      addField (ctemplate "$1 Disc $2;" [modifiers,discVar])
      addField (ctemplate "$1 Object $2;" [modifiers,valueVar])

      -- Discriminator enum
      let terminators = replicate (length fieldDetails-1) "," <> [""]
          discdef =
            docStringComment (template "The $1 discriminator type." [className])
            <>
            cblock "public enum Disc" (
              mconcat [ctemplate "$1$2" [discriminatorName fd,term]
                      | (fd,term) <- zip fieldDetails terminators]
               )
      addMethod discdef

      -- constructors
      addMethod (cline "/* Constructors */")
      
      for_ fieldDetails $ \fd -> do
        let checkedv = if needsNullCheck fd then "java.util.Objects.requireNonNull(v)" else "v"
            ctor = cblock (template "public static$1 $2 $3($4 v)" [leadSpace typeArgs, className, unionCtorName (fd_field fd), fd_typeExprStr fd]) (
              ctemplate "return new $1(Disc.$2, $3);" [className, discriminatorName fd, checkedv]
              )
            ctorvoid = cblock (template "public static$1 $2 $3()" [leadSpace typeArgs, className, unionCtorName (fd_field fd)]) (
              ctemplate "return new $1(Disc.$2, null);" [className, discriminatorName fd]
              )

        addMethod (if isVoidType (f_type (fd_field fd)) then ctorvoid else ctor)

      let ctorPrivate = cblock (template "private $1(Disc disc, Object value)" [className]) (
            ctemplate "this.$1 = disc;" [discVar]
            <>
            ctemplate "this.$1 = value;" [valueVar]
            )

          ctorDefault = cblock (template "public $1()" [className]) (
            ctemplate "this.$1 = Disc.$2;" [discVar,discriminatorName fieldDetail0]
            <>
            ctemplate "this.$1 = $2;" [valueVar,literalValue (fd_defValue fieldDetail0)]
            )

          ctorCopy = cblock (template "public $1($1 other)" [className]) (
            ctemplate "this.$1 = other.$1;" [discVar]
            <>
            cblock (template "switch (other.$1)" [discVar]) (
              mconcat [
                ctemplate "case $1:" [discriminatorName fd]
                <>
                indent (
                  ctemplate "this.$1 = $2;" [valueVar,fd_copy fd (template "($1) other.$2" [fd_boxedTypeExprStr fd,valueVar])]
                  <>
                  cline "break;"
                  )
                | fd <- fieldDetails]
              )
            )

      when (not isGeneric) $ do
          addMethod ctorDefault
          addMethod ctorCopy
      addMethod $ ctorPrivate

      -- accessors
      addMethod (cline "/* Accessors */")

      addMethod $ cblock "public Disc getDisc()" (
        ctemplate "return $1;" [discVar]
        )

      for_ fieldDetails $ \fd -> do
        let getter = cblock (template "public $1 get$2()" [fd_typeExprStr fd, javaCapsFieldName (fd_field fd)]) (
              cblock (template "if ($1 == Disc.$2)" [discVar,discriminatorName fd]) (
                 ctemplate "return cast($1);" [valueVar]
                 )
              <>
              cline "throw new IllegalStateException();"
              )

        when (not (isVoidType (f_type (fd_field fd)))) (addMethod getter)

      -- mutators
      addMethod (cline "/* Mutators */")

      when (cgp_mutable codeProfile) $ do 
        for_ fieldDetails $ \fd -> do
          let checkedv = if needsNullCheck fd then "java.util.Objects.requireNonNull(v)" else "v"
              mtor = cblock (template "public void set$1($2 v)" [javaCapsFieldName (fd_field fd), fd_typeExprStr fd]) (
                ctemplate "this.$1 = $2;" [valueVar,checkedv]
                <>
                ctemplate "this.$1 = Disc.$2;" [discVar,discriminatorName fd]
                )
              mtorvoid = cblock (template "public void set$1()" [javaCapsFieldName (fd_field fd)]) (
                ctemplate "this.$1 = null;" [valueVar]
                <>
                ctemplate "this.$1 = Disc.$2;" [discVar,discriminatorName fd]
                )
          addMethod (if isVoidType (f_type (fd_field fd)) then mtorvoid else mtor)

      -- equals and hashcode
      addMethod (cline "/* Object level helpers */")

      addMethod $ coverride "public boolean equals(Object other0)" (
        cblock (template "if (!(other0 instanceof $1))"  [className]) (
          cline "return false;"
          )
        <>
        ctemplate "$1 other = ($1) other0;" [className]
        <>
        ctemplate "return $1 == other.$1 && $2.equals(other.$2);" [discVar,valueVar]
        )

      addMethod $ coverride "public int hashCode()" (
        ctemplate "return $1.hashCode() * 37 + $2.hashCode();" [discVar,valueVar]
        )

      -- cast helper
      addMethod (
        cline "@SuppressWarnings(\"unchecked\")"
        <>
        cblock "private static <T> T cast(final Object o)" (
          cline "return (T) o;"
          )
        )

      -- factory
      let factory =
            cblock1 (template "public static final Factory<$1> FACTORY = new Factory<$1>()" [className]) (
              cblock (template "public $1 create()" [className]) (
                 ctemplate "return new $1();" [className]
              )
              <>
              cblock (template "public $1 create($1 other)" [className]) (
                 ctemplate "return new $1(other);" [className]
              )
            )

      let factoryg =
            cblock (template "public static$2 Factory<$1$2> factory($3)" [className,leadSpace typeArgs,factoryArgs]) (
              cblock1 (template "return new Factory<$1$2>()" [className,typeArgs]) (
                mconcat [ctemplate "final Factory<$1> $2 = $3;" [fd_boxedTypeExprStr fd,fd_fieldName fd,fd_factoryExprStr fd] | fd <- fieldDetails, not (immutableType (f_type (fd_field fd)))]
                <>
                cline ""
                <>
                cblock (template "public $1$2 create()" [className,typeArgs]) (
                  let val = if immutableType (f_type (fd_field fieldDetail0))
                            then literalValue (fd_defValue fieldDetail0)
                            else template "$1.create()" [fd_fieldName fieldDetail0]
                  in ctemplate "return new $1$2(Disc.$3,$4);" [className,typeArgs,discriminatorName fieldDetail0,val]
                )
                <>
                cline ""
                <>
                cblock (template "public $1$2 create($1$2 other)" [className,typeArgs]) (
                  cline "Object value = null;"
                  <>
                  cblock (template "switch (other.$1)" [discVar]) (
                    mconcat [
                      ctemplate "case $1:" [discriminatorName fd]
                      <>
                      indent (
                        ctemplate "value = $1;"
                          [if immutableType (f_type (fd_field fd))
                           then "other.value"
                           else template "$1.create(cast(other.value))" [fd_fieldName fd,fd_boxedTypeExprStr fd]
                          ]
                        <>
                        cline "break;"
                        )
                      | fd <- fieldDetails]
                    )
                  <>
                  ctemplate "return new $1$2(other.$3,$4);" [className,typeArgs,discVar,valueVar]
                  )
                )
              )

          factoryArgs = commaSep [template "Factory<$1> $2" [arg,factoryTypeArg arg] | arg <- u_typeParams union]

      addMethod (cline "/* Factory for construction of generic values */")
      addImport (cgp_runtimePackage codeProfile <> ".Factory")
      addMethod (if isGeneric then factoryg else factory)

      -- Parcelable
      when (cgp_parcelable codeProfile) $ do
        importParcelable
        addImplements "Parcelable"

        addMethod (cline "/* Android Parcelable implementation */")
        
        addMethod $ coverride "public int describeContents()" (
          cline "return 0;"
          )

        writeFields <- for fieldDetails $ \fd -> do
          writeToParcel (f_type (fd_field fd)) "out" (template "(($1) $2)" [fd_typeExprStr fd,valueVar]) "flags"

        readFields <- for fieldDetails $ \fd -> do
          readFromParcel (f_type (fd_field fd)) Nothing "value" "in"

        addMethod $ coverride "public void writeToParcel(Parcel out, int flags)" (
          ctemplate "out.writeInt($1.ordinal());" [discVar]
          <>
          cblock (template "switch($1)" [discVar]) (
            mconcat [
              ctemplate "case $1:" [discriminatorName fd]
              <>
              indent (
                writeField
                <>
                cline "break;"
                )
              | (fd,writeField) <- zip fieldDetails writeFields]
            )
          )

        addMethod $ cblock1 (template "public static final Parcelable.Creator<$1> CREATOR = new Parcelable.Creator<$1>()" [className]) (
          coverride (template "public $1 createFromParcel(Parcel in)" [className]) (
            cline "Disc disc = Disc.values()[in.readInt()];"
            <>
            cline "Object value = null;"
            <>
            cblock "switch(disc)" (
              mconcat [
                ctemplate "case $1:" [discriminatorName fd]
                <>
                indent (
                  readField
                  <>
                  cline "break;"
                  )
                | (fd,readField) <- zip fieldDetails readFields]
              )
            <>
            ctemplate "return new $1(disc, value);" [className]
            )
          <>
          cline ""
          <>
          coverride (template "public $1[] newArray(int size)" [className]) (
            ctemplate "return new $1[size];" [className]
            )
          )

generateDocString :: Annotations -> Code
generateDocString annotations = case Map.lookup (ScopedName (ModuleName []) "Doc") annotations of
   (Just (JSON.String text)) -> docStringComment text
   _ -> mempty

docStringComment :: T.Text -> Code
docStringComment text = cline "/**" <> mconcat [cline (" * " <> line) | line <- T.lines text] <> cline " */"

multiLineComment :: T.Text -> Code
multiLineComment text = cline "/*" <> mconcat [cline (" * " <> line) | line <- T.lines text] <> cline " */"

literalValue :: Literal -> T.Text
literalValue (LDefault t _) = template "new $1()" [t]
literalValue (LCtor t _ ls) = template "new $1($2)" [t, T.intercalate ", " (map literalValue ls)]
literalValue (LUnion t ctor l) = template "$1.$2($3)" [t, ctor, literalValue l ]
literalValue (LVector t ls) = template "java.util.Arrays.asList($1)" [commaSep (map literalValue ls)]
literalValue (LPrimitive _ t) = t

packageGenerator :: T.Text -> ModuleName -> JavaPackage
packageGenerator basePackage mn = JavaPackage (T.splitOn "." basePackage <> unModuleName mn)

fileGenerator :: T.Text -> ScopedName -> FilePath
fileGenerator basePackage sn = T.unpack (T.intercalate "/" idents <> ".java")
  where
    idents = unJavaPackage (packageGenerator basePackage (sn_moduleName sn)) <> [sn_name sn]

generate :: JavaFlags -> [FilePath] -> EIOT ()
generate jf modulePaths = catchAllExceptions  $ for_ modulePaths $ \modulePath -> do
  m <- loadAndCheckModule (moduleFinder (jf_searchPath jf)) modulePath
  generateModule (packageGenerator (jf_package jf))
                 (fileGenerator (jf_package jf))
                 (const (jf_codeGenProfile jf))
                 (jf_fileWriter jf)
                 m

commaSep :: [T.Text] -> T.Text
commaSep = T.intercalate ", "

sortedImports :: Set.Set T.Text -> [T.Text]
sortedImports imports = map snd (sort [(classify i,i) | i <- Set.toList imports])
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
 , "Factory"
 ]

unreserveWord :: Ident -> Ident
unreserveWord n | Set.member n reservedWords = T.append n "_"
                | otherwise = n

javaCapsFieldName :: Field ResolvedType -> Ident
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

discriminatorName :: FieldDetails -> Ident
discriminatorName = T.toUpper . unreserveWord . f_name . fd_field

leadSpace :: T.Text -> T.Text
leadSpace "" = ""
leadSpace t = " " <> t

----------------------------------------------------------------------
-- A trivial DSL for generated indented block structured text

data Code = CEmpty
          | CLine T.Text      
          | CAppend Code Code
          | CIndent Code

instance Monoid Code where
  mempty = CEmpty
  mappend = CAppend

cline :: T.Text -> Code
cline t = CLine t

clineN :: [T.Text] -> Code
clineN ts = mconcat (map CLine ts)

indent :: Code -> Code
indent = CIndent

indentN :: [Code] -> Code
indentN = CIndent . mconcat

cblock :: T.Text -> Code -> Code
cblock "" body = cline "{"  <> indent body <> cline "}"
cblock intro body =  cline (intro <> " {")  <> indent body <> cline "}"
  

cblock1 :: T.Text -> Code -> Code
cblock1 intro body =
  cline (intro <> " {")  <> indent body <> cline "};"

coverride :: T.Text -> Code -> Code
coverride intro body =
  cline "@Override" <> cline (intro <> " {")  <> indent body <> cline "}"

ctemplate :: T.Text -> [T.Text] -> Code
ctemplate pattern params = cline $ template pattern params

codeText :: Int -> Code -> [T.Text]
codeText maxLineLength c = mkLines "" c
  where
    mkLines i CEmpty = []
    mkLines i (CAppend c1 c2) = mkLines i c1 <> mkLines i c2
    mkLines i (CIndent c) = mkLines (indentStr <> i) c
    mkLines i (CLine "") = [""]
    mkLines i (CLine t) = case breakLine (maxLineLength - T.length i) t of
      [] -> []
      (l1:ls) -> (i <> l1):[i <> i <> l | l <- ls]
    indentStr = "  "

breakLine :: Int -> T.Text -> [T.Text]
breakLine maxlength t
  | T.length t < maxlength = [t]
  | otherwise = map (T.strip . T.concat) (assemble 0 [] (lineBreakChunks t))
  where
    assemble len cline [] = [cline]
    assemble len cline (c:cs)
      | len + T.length c > maxlength = case cline of
           [] -> assemble (T.length c) [c] cs
           _ -> cline : assemble (T.length c) [c] cs
      | otherwise = assemble (len + T.length c) (cline++[c]) cs


lineBreakChunks :: T.Text -> [T.Text]
lineBreakChunks t = map (T.pack . reverse) (chunks "" (T.unpack t))
  where
    -- We break after ',' '=', or '(', but never within                                                                      
    -- strings                                                                                                                       
    chunks cs [] = [cs]
    chunks cs ('\'':s) = quote1 ('\'':cs) s                                                                                          
    chunks cs ('\"':s) = quote2 ('\"':cs) s
    chunks cs (',':s) = (',':cs) : chunks "" s
    chunks cs ('=':s) = ('=':cs) : chunks "" s
    chunks cs ('(':s) = ('(':cs) : chunks "" s
    chunks cs (c:s) =  chunks (c:cs) s

    quote1 cs [] = [cs]
    quote1 cs ('\'':s) = chunks ('\'':cs) s
    quote1 cs ('\\':'\'':s) = quote1 ('\\':'\'':cs) s
    quote1 cs (c:s) =  quote1 (c:cs) s

    quote2 cs [] = [cs]
    quote2 cs ('\"':s) = chunks ('\"':cs) s
    quote2 cs ('\\':'\"':s) = quote2 ('\\':'\"':cs) s
    quote2 cs (c:s) =  quote2 (c:cs) s
