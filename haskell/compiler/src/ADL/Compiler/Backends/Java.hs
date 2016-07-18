{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Java(
  generate,
  JavaFlags(..)
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
import Data.List(intersperse,replicate)
import Data.Monoid

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
  jf_fileWriter :: FilePath -> LBS.ByteString -> IO ()
  }

newtype JavaPackage = JavaPackage {
  unJavaPackage :: [Ident]
}

genJavaPackage :: JavaPackage -> T.Text
genJavaPackage package = T.intercalate "." (map unreserveWord (unJavaPackage package))

data CodeGenProfile = CodeGenProfile {
  cgp_mutable :: Bool,
  cgp_publicMembers :: Bool,
  cgp_genericFactories :: Bool
}

defaultCodeGenProfile = CodeGenProfile True False False

data ClassFile = ClassFile {
   cf_module :: ModuleName,
   cf_package :: JavaPackage,
   cf_imports :: Set.Set T.Text,
   cf_decl :: T.Text,
   cf_fields :: [Code],
   cf_methods :: [Code]
}

classFile :: ModuleName -> JavaPackage -> T.Text -> ClassFile
classFile mname javapackage decl = ClassFile mname javapackage Set.empty decl [] []

classFileCode :: ClassFile -> Code
classFileCode content =
  ctemplate "package $1;" [genJavaPackage (cf_package content)]
  <>
  cline ""
  <>
  mconcat [ctemplate "import $1;" [imp] | imp <- Set.toList (cf_imports content)]
  <>
  cline ""
  <>
  cblock (template "$1" [cf_decl content]) (
    cline ""
    <>
    mconcat (reverse (cf_fields content))
    <>
    cline ""
    <>
    mconcat (intersperse (cline "") (reverse (cf_methods content)))
  )

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
  getUnionConstructorName d f = return "FIXME"

addField :: Code -> CState ()
addField decl = modify (\cf->cf{cf_fields=decl:cf_fields cf})

addMethod :: Code -> CState ()
addMethod method = modify (\cf->cf{cf_methods=method:cf_methods cf})

addImport :: T.Text -> CState ()
addImport imp = modify (\cf->cf{cf_imports=Set.insert imp (cf_imports cf)})

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
      return (template "$1.factory" [fe])
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

numPrimitive :: T.Text -> T.Text -> PrimitiveDetails
numPrimitive unboxed boxed = PrimitiveDetails {
  pd_unboxed = Just (return unboxed),
  pd_type = return boxed,
  pd_default = Just "0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n,
  pd_mutable = False,
  pd_factory = primitiveFactory boxed,
  pd_hashfn = \from -> template "(int)$1" [from]
  }
  
genPrimitiveDetails :: PrimitiveType -> PrimitiveDetails
genPrimitiveDetails P_Void = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "Void",
  pd_default = Just "null",
  pd_genLiteral = \jv -> "null",
  pd_mutable = False,
  pd_factory = primitiveFactory "Void",
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
  pd_factory = primitiveFactory "Boolean",
  pd_hashfn = \from -> template "($1 ? 0 : 1)" [from]
  }
genPrimitiveDetails P_Int8 = numPrimitive "byte" "Byte"
genPrimitiveDetails P_Int16 = numPrimitive "short" "Short"
genPrimitiveDetails P_Int32 = numPrimitive "int" "Integer"
genPrimitiveDetails P_Int64 = (numPrimitive "long" "Long") {
  pd_hashfn = \from -> template "(int)($1 ^ ($1 >>> 32))" [from]
}
genPrimitiveDetails P_Word8 = genPrimitiveDetails P_Int8
genPrimitiveDetails P_Word16 = genPrimitiveDetails P_Int16
genPrimitiveDetails P_Word32 = genPrimitiveDetails P_Int32
genPrimitiveDetails P_Word64 = genPrimitiveDetails P_Int64

genPrimitiveDetails P_Float = numPrimitive "float" "Float"
genPrimitiveDetails P_Double = numPrimitive "double" "Double"
genPrimitiveDetails P_ByteVector = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "java.nio.ByteBuffer",
  pd_default = Just "java.nio.ByteBuffer.allocate(0)",
  pd_genLiteral = \(JSON.String s) -> template "java.nio.ByteBuffer.allocate(0).put($1.getBytes())" [T.pack (show (decode s))],
  pd_mutable = True,
  pd_factory = primitiveFactory "ByteBuffer",
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
  pd_factory = primitiveFactory "ArrayList",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_String = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "String",
  pd_default = Just "\"\"",
  pd_genLiteral = \(JSON.String s) -> T.pack (show s),
  pd_mutable= False,
  pd_factory = primitiveFactory "String",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_Sink = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "Sink",
  pd_default = Just "new Sink()",
  pd_genLiteral = \_ -> "????", -- never called
  pd_mutable = True,
  pd_factory = primitiveFactory "Sink",
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }

primitiveFactory :: T.Text -> CState T.Text
primitiveFactory name = addImport "org.adl.runtime.Factories" >> return (template "Factories.$1Factory" [name])

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

  let copy from =
        if immutableType te
          then from
          else template "$1.create($2)" [factoryExprStr,from]

  return (FieldDetails f (unreserveWord (f_name f)) typeExprStr boxedTypeExprStr factoryExprStr litv copy)



generateModule :: (ModuleName -> JavaPackage) ->
                  (ScopedName -> FilePath) ->
                  (ScopedName -> CodeGenProfile) ->
                  (FilePath -> LBS.ByteString -> IO ()) ->
                  Module ResolvedType ->
                  EIO a ()
generateModule mPackage mFile mCodeGetProfile fileWriter m = do
  let decls = Map.elems (m_decls m)
  for_ decls $ \decl -> do
    let moduleName = m_name m
        javaPackage = mPackage moduleName
        codeProfile = mCodeGetProfile (ScopedName moduleName (d_name decl))
        file = mFile (ScopedName moduleName (unreserveWord (d_name decl)))
    case d_type decl of
      (Decl_Struct s) -> writeClassFile file (generateStruct codeProfile moduleName javaPackage decl s)
      (Decl_Union u) -> let s = Struct (u_typeParams u) (u_fields u)  -- FIXME: hack to get some sort out output for unions
                         in writeClassFile file (generateStruct codeProfile moduleName javaPackage decl s)
      _ -> return ()
  where
    writeClassFile :: FilePath -> ClassFile -> EIO a ()
    writeClassFile path cfile = do
      let lines = codeText (classFileCode cfile)
      liftIO $ fileWriter path (LBS.fromStrict (T.encodeUtf8 (T.intercalate "\n" lines)))
      

generateStruct :: CodeGenProfile -> ModuleName -> JavaPackage -> Decl ResolvedType -> Struct ResolvedType -> ClassFile
generateStruct codeProfile moduleName javaPackage decl struct =  execState gen state0
  where
    state0 = classFile moduleName javaPackage classDecl
    className = unreserveWord (d_name decl)
    classDecl = "public class " <> className <> typeArgs
    typeArgs = case s_typeParams struct of
      [] -> ""
      args -> "<" <> commaSep (map unreserveWord args) <> ">"
    gen = do
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
                if unboxedField fd || fd_typeExprStr fd == "Void"
                  then template "this.$1 = $1;" [fd_fieldName fd]
                  else template "this.$1 = java.util.Objects.requireNonNull($1);" [fd_fieldName fd]
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

      addMethod ctor1
      when (not isGeneric) $ do
          addMethod ctor2
          addMethod ctor3

      -- Getters/Setters
      when (not (cgp_publicMembers codeProfile)) $ do
        for_ fieldDetails $ \fd -> do
          let fieldName = fd_fieldName fd
              capsFieldName = javaCapsFieldName fd
              typeExprStr = fd_typeExprStr fd
              getter =
                cblock (template "public $1 get$2()" [typeExprStr,capsFieldName]) (
                  ctemplate "return $1;" [fieldName]
                )
              setter =
                cblock (template "public void set$1($2 new$1)" [capsFieldName,typeExprStr]) (
                  ctemplate "$1 = new$2;" [fieldName,capsFieldName]
                )
          addMethod getter
          when (cgp_mutable codeProfile) (addMethod setter)

      -- equals
      addMethod $ cblock (template "public boolean equals($1 other)"[className]) (
        cline "return"
        <>
        let terminators = replicate (length fieldDetails-1) " &&" <> [";"]
            tests = [ctemplate (if unboxedField fd then "$1 == other.$1$2" else "$1.equals(other.$1)$2")
                               [fd_fieldName fd,term]
                    | (fd,term) <- zip fieldDetails terminators]
        in  indent (mconcat tests)
        )

      -- hashcode
      addMethod $ cblock "public int hashCode()" (
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
            cblock1 (template "public static Factory<$1> factory = new Factory<$1>()" [className]) (
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

      addImport "org.adl.runtime.Factory"
      addMethod (if isGeneric then factoryg else factory)
          
          
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
                 (const defaultCodeGenProfile)
                 (jf_fileWriter jf)
                 m

commaSep :: [T.Text] -> T.Text
commaSep = T.intercalate ","

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

javaCapsFieldName :: FieldDetails -> Ident
javaCapsFieldName fd = case T.uncons (f_name (fd_field fd)) of
  Nothing -> ""
  (Just (c,t)) -> T.cons (toUpper c) t

factoryTypeArg :: Ident -> Ident
factoryTypeArg n = "factory" <> n

fieldAccessExpr :: CodeGenProfile -> FieldDetails -> Ident
fieldAccessExpr cgp fd
  | cgp_publicMembers cgp = fd_fieldName fd
  | otherwise = template "get$1()" [javaCapsFieldName fd]

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
cblock intro body =
  cline (intro <> " {")  <> indent body <> cline "}"

cblock1 :: T.Text -> Code -> Code
cblock1 intro body =
  cline (intro <> " {")  <> indent body <> cline "};"

ctemplate :: T.Text -> [T.Text] -> Code
ctemplate pattern params = cline $ template pattern params

codeText :: Code -> [T.Text]
codeText c = mkLines "" c
  where
    mkLines i CEmpty = []
    mkLines i (CLine "") = [""]
    mkLines i (CLine t) = [i <> t]
    mkLines i (CAppend c1 c2) = mkLines i c1 <> mkLines i c2
    mkLines i (CIndent c) = mkLines (indentStr <> i) c
    indentStr = "  "
    

