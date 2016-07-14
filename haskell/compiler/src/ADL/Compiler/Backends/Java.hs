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
   cf_decl :: T.Text,
   cf_fields :: [Code],
   cf_methods :: [Code]
}

classFile :: ModuleName -> JavaPackage -> T.Text -> ClassFile
classFile mname javapackage decl = ClassFile mname javapackage decl [] []

classFileCode :: ClassFile -> Code
classFileCode content =
  ctemplate "package $1;" [genJavaPackage (cf_package content)]
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

data PrimitiveDetails = PrimitiveDetails {
  pd_type :: CState T.Text,
  pd_unboxed :: Maybe (CState T.Text),
  pd_default :: Maybe T.Text,
  pd_genLiteral :: JSON.Value -> T.Text,
  pd_copy :: T.Text -> T.Text -> Code,
  pd_hashfn :: T.Text -> T.Text
}

numPrimitive :: T.Text -> T.Text -> PrimitiveDetails
numPrimitive unboxed boxed = PrimitiveDetails {
  pd_unboxed = Just (return unboxed),
  pd_type = return boxed,
  pd_default = Just "0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n,
  pd_copy = assignValue,
  pd_hashfn = \from -> template "(int)$1" [from]
  }
  
genPrimitiveDetails :: PrimitiveType -> PrimitiveDetails
genPrimitiveDetails P_Void = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "Void",
  pd_default = Just "null",
  pd_genLiteral = \jv -> "null",
  pd_copy = assignValue,
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
  pd_copy = assignValue,
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
  pd_copy = \to from -> ctemplate "$1 = $2.duplicate();" [to,from],
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
  pd_copy = \to from -> ctemplate "$1 = new java.util.ArrayList($1);" [to,from], -- FIXME should be a deep copy
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_String = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "String",
  pd_default = Just "\"\"",
  pd_genLiteral = \(JSON.String s) -> T.pack (show s),
  pd_copy = assignValue,
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }
genPrimitiveDetails P_Sink = PrimitiveDetails {
  pd_unboxed = Nothing,
  pd_type = return "Sink",
  pd_default = Just "new Sink()",
  pd_genLiteral = \_ -> "????", -- never called
  pd_copy = assignValue,
  pd_hashfn = \from -> template "$1.hashCode()" [from]
  }

assignValue :: T.Text -> T.Text -> Code
assignValue to from = ctemplate "$1 = $2;" [to,from]

data FieldDetails = FieldDetails {
  fd_field :: Field ResolvedType,
  fd_fieldName :: Ident,
  fd_typeExprStr :: T.Text,
  fd_defValue :: Literal,
  fd_copy :: T.Text -> T.Text -> Code
}

unboxedField fd = case (f_type (fd_field fd)) of
  (TypeExpr (RT_Primitive pt) []) -> isJust (pd_unboxed (genPrimitiveDetails pt))
  _ -> False

genFieldDetails :: Field ResolvedType -> CState FieldDetails
genFieldDetails f = do
  typeExprStr <- genTypeExpr te
  litv <- case f_default f of
    (Just v) -> mkLiteral te v
    Nothing -> mkDefaultLiteral te
  return (FieldDetails f (unreserveWord (f_name f)) typeExprStr litv (copy typeExprStr))
  where
    te = f_type f

    copy typeExprStr to from = case te of
      (TypeExpr (RT_Primitive pt) []) ->
        pd_copy (genPrimitiveDetails pt) to from
      (TypeExpr (RT_Param i) _) ->
        ctemplate "$1 = $2.getDeclaredConstructor($3).newInstance($4);" [to,classArgName i,argTypes from,argValues from]
      _ -> 
        ctemplate "$1 = new $2($3);" [to,typeExprStr,argValues from]

    argValues from = commaSep ([from] <> getClassArgNames te)
    argTypes from = commaSep ([from <> ".getClass()"] <> ["Class.forName(\"java.lang.Class\")" | _ <- getClassArgNames te])



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
          classArgs = T.intercalate ", " [template "Class<$1> $2" [typeParam,classArgName typeParam]
                                         | typeParam <- s_typeParams struct] 
          isGeneric = length (s_typeParams struct) > 0
          
          ctor1 =
            cblock (template "public $1($2)" [className,ctorArgs]) (
              clineN [
                if unboxedField fd
                  then template "this.$1 = $1;" [fd_fieldName fd]
                  else template "this.$1 = java.util.Objects.requireNonNull($1);" [fd_fieldName fd]
                | fd <- fieldDetails]
            )

          ctor2 =
            cblock (template "public $1()" [className]) (
              clineN [template "this.$1 = $2;" [fd_fieldName fd,literalValue (fd_defValue fd)] | fd <- fieldDetails]
            )

          ctor2g =
            cblock (template "public $1($2)" [className,classArgs]) (
              cblock "try" ( 
                clineN [template "this.$1 = $2;" [fd_fieldName fd,literalValue (fd_defValue fd)] | fd <- fieldDetails]
              ) <> 
              cblock "catch (ReflectiveOperationException e)" (
                cline "throw new RuntimeException(e);"
              )
            )
            
          ctor3 =
            cblock (template "public $1($2 other)" [className, className <> typeArgs]) (
              mconcat [ let n = fd_fieldName fd in fd_copy fd ("this."<>n) ("other."<>n)
                      | fd <- fieldDetails ]
            )

          ctor3g =
            cblock (template "public $1($2 other, $3)" [className, className <> typeArgs, classArgs]) (
              cblock "try" ( 
                mconcat [ let n = fd_fieldName fd in fd_copy fd ("this."<>n) ("other."<>n)
                        | fd <- fieldDetails ]
              )
              <> 
              cblock "catch (ReflectiveOperationException e)" (
                cline "throw new RuntimeException(e);"
              )
            )

      addMethod ctor1
      if isGeneric
        then when (cgp_genericFactories codeProfile) $ do
          addMethod ctor2g
          addMethod ctor3g
        else do
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
          
          
literalValue :: Literal -> T.Text
literalValue (LDefault t te) = template "$1($2)" [ctor te,commaSep (getClassArgNames te)]
  where
    ctor (TypeExpr (RT_Param i) _) = template "$1.getDeclaredConstructor($2).newInstance" [classArgName i,commaSep (getCtorArgTypes te)]
    ctor _ = template "new $1" [t]

literalValue (LCtor t _ ls) = template "new $1($2)" [t, T.intercalate ", " (map literalValue ls)]
literalValue (LUnion t ctor l) = template "$1.$2($3)" [t, ctor, literalValue l ]
literalValue (LVector t ls) = template "java.util.Arrays.asList($1)" [commaSep (map literalValue ls)] -- FIXME
literalValue (LPrimitive _ t) = t

getClassArgNames :: TypeExpr ResolvedType -> [Ident]
getClassArgNames (TypeExpr (RT_Primitive _) targs) = []
getClassArgNames (TypeExpr _ targs) = [classArgName i | (TypeExpr (RT_Param i) _) <- targs]

getCtorArgTypes :: TypeExpr ResolvedType -> [Ident]
getCtorArgTypes (TypeExpr _ targs) = ["FIXME" | targ <- targs]

classArgName :: Ident -> Ident
classArgName i = "class"<>i

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
 ]

unreserveWord :: Ident -> Ident
unreserveWord n | Set.member n reservedWords = T.append n "_"
                | otherwise = n

javaCapsFieldName :: FieldDetails -> Ident
javaCapsFieldName fd = case T.uncons (f_name (fd_field fd)) of
  Nothing -> ""
  (Just (c,t)) -> T.cons (toUpper c) t


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
    

