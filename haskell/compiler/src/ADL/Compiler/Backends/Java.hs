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
import Data.Foldable(for_)
import Data.List(intersperse)
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
genJavaPackage package = T.intercalate "." (unJavaPackage package)

data CodeGenProfile = CodeGenProfile {
  cgp_mutable :: Bool,
  cgp_publicMembers :: Bool,
  cgp_genericFactories :: Bool
}

defaultCodeGenProfile = CodeGenProfile True True False

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
  getPrimitiveType = pd_unboxed . genPrimitiveDetails 
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
genResolvedType _(RT_Param ident) = return ident
genResolvedType False (RT_Primitive pt) = pd_unboxed (genPrimitiveDetails pt)
genResolvedType True (RT_Primitive pt) = pd_boxed (genPrimitiveDetails pt)

genScopedName :: ScopedName -> CState T.Text
genScopedName scopedName = do
  currentModuleName <- fmap cf_module get
  let mname = sn_moduleName scopedName
  if mname  == currentModuleName
    then return (sn_name scopedName)
    else return (T.intercalate "." (unModuleName mname) <> sn_name scopedName)

data PrimitiveDetails = PrimitiveDetails {
  pd_unboxed :: CState T.Text,
  pd_boxed :: CState T.Text,
  pd_default :: Maybe T.Text,
  pd_genLiteral :: JSON.Value -> T.Text,
  pd_copy :: T.Text -> T.Text -> Code
}

numPrimitive :: T.Text -> T.Text -> PrimitiveDetails
numPrimitive unboxed boxed = PrimitiveDetails {
  pd_unboxed = return unboxed,
  pd_boxed = return boxed,
  pd_default = Just "0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n,
  pd_copy = assignValue
  }
  
genPrimitiveDetails :: PrimitiveType -> PrimitiveDetails
genPrimitiveDetails P_Void = PrimitiveDetails {
  pd_unboxed = return "Void",
  pd_boxed = return "Void",
  pd_default = Just "null",
  pd_genLiteral = \jv -> "null",
  pd_copy = assignValue
  }
genPrimitiveDetails P_Bool = PrimitiveDetails {
  pd_unboxed = return "boolean",
  pd_boxed = return "Boolean",
  pd_default = Just "false",
  pd_genLiteral = \jv ->
    case jv of
      (JSON.Bool True) -> "true"
      (JSON.Bool False) -> "false",
  pd_copy = assignValue
  }
genPrimitiveDetails P_Int8 = numPrimitive "byte" "Byte"
genPrimitiveDetails P_Int16 = numPrimitive "short" "Short"
genPrimitiveDetails P_Int32 = numPrimitive "int" "Int"
genPrimitiveDetails P_Int64 = numPrimitive "long" "Long"
genPrimitiveDetails P_Word8 = numPrimitive "byte" "Byte"
genPrimitiveDetails P_Word16 = numPrimitive "short" "Short"
genPrimitiveDetails P_Word32 = numPrimitive "int" "Int"
genPrimitiveDetails P_Word64 = numPrimitive "long" "Long"
genPrimitiveDetails P_Float = numPrimitive "float" "Float"
genPrimitiveDetails P_Double = numPrimitive "double" "Double"
genPrimitiveDetails P_ByteVector = PrimitiveDetails {
  pd_unboxed = return "byte[]",
  pd_boxed = return "byte[]",
  pd_default = Just "new byte[0]",
  pd_genLiteral = \(JSON.String s) -> template "$1.getBytes()" [T.pack (show (decode s))],
  pd_copy = \to from -> ctemplate "$1 = java.util.Arrays.copyOf($2, $2.length);" [to,from]
  }
  where
    decode s = case B64.decode (T.encodeUtf8 s) of
      (Left _) -> "???"
      (Right s) -> s
genPrimitiveDetails P_Vector = PrimitiveDetails {
  pd_unboxed = return "java.util.ArrayList",
  pd_boxed = return "java.util.ArrayList",
  pd_default = Just "new java.util.ArrayList()",
  pd_genLiteral = \(JSON.String s) -> "???", -- never called
  pd_copy = \to from -> ctemplate "$1 = new java.util.ArrayList($1);" [to,from] -- FIXME should be a deep copy
  }
genPrimitiveDetails P_String = PrimitiveDetails {
  pd_unboxed = return "String",
  pd_boxed = return "String",
  pd_default = Just "\"\"",
  pd_genLiteral = \(JSON.String s) -> T.pack (show s),
  pd_copy = assignValue
  }
genPrimitiveDetails P_Sink = PrimitiveDetails {
  pd_unboxed = return "Sink",
  pd_boxed = return "Sink",
  pd_default = Just "new Sink()",
  pd_genLiteral = \_ -> "????", -- never called
  pd_copy = assignValue
  }

assignValue :: T.Text -> T.Text -> Code
assignValue to from = ctemplate "$1 = $2;" [to,from]

data FieldDetails = FieldDetails {
  fd_field :: Field ResolvedType,
  fd_typeExprStr :: T.Text,
  fd_defValue :: Literal,
  fd_copy :: T.Text -> T.Text -> Code
}

genFieldDetails :: Field ResolvedType -> CState FieldDetails
genFieldDetails f = do
  typeExprStr <- genTypeExpr te
  litv <- case f_default f of
    (Just v) -> mkLiteral te v
    Nothing -> mkDefaultLiteral te
  return (FieldDetails f typeExprStr litv (copy typeExprStr))
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
        scopedName = ScopedName moduleName (d_name decl)
        javaPackage = mPackage moduleName
        codeProfile = mCodeGetProfile scopedName
        file = mFile scopedName
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
    classDecl = "public class " <> d_name decl <> typeArgs
    typeArgs = case s_typeParams struct of
      [] -> ""
      args -> "<" <> commaSep args <> ">"
    gen = do
      fieldDetails <- mapM genFieldDetails (s_fields struct)

      -- Fields
      for_ (s_fields struct) $ \field -> do
        typeExpr <- genTypeExpr (f_type field)
        let modifiers =
             (if cgp_publicMembers codeProfile then ["public"] else [])
             <>
             (if cgp_mutable codeProfile then [] else ["final"])
        addField (ctemplate "$1 $2 $3;" [T.intercalate " " modifiers,typeExpr,f_name field])

      -- Constructors
      let ctorArgs =  T.intercalate ", " [fd_typeExprStr fd <> " " <> f_name (fd_field fd) | fd <- fieldDetails]
          classArgs = T.intercalate ", " [template "Class<$1> $2" [typeParam,classArgName typeParam]
                                         | typeParam <- s_typeParams struct] 
          isGeneric = length (s_typeParams struct) > 0
          
          ctor1 =
            cblock (template "public $1($2)" [d_name decl,ctorArgs]) (
              clineN [template "this.$1 = $1;" [f_name f] | f <- s_fields struct]
            )

          ctor2 =
            cblock (template "public $1()" [d_name decl]) (
              clineN [template "this.$1 = $2;" [f_name (fd_field fd),literalValue (fd_defValue fd)] | fd <- fieldDetails]
            )

          ctor2g =
            cblock (template "public $1($2)" [d_name decl,classArgs]) (
              cblock "try" ( 
                clineN [template "this.$1 = $2;" [f_name (fd_field fd),literalValue (fd_defValue fd)] | fd <- fieldDetails]
              ) <> 
              cblock "catch (ReflectiveOperationException e)" (
                cline "throw new RuntimeException(e);"
              )
            )
            
          ctor3 =
            cblock (template "public $1($2 other)" [d_name decl, d_name decl <> typeArgs]) (
              mconcat [ let n = f_name (fd_field fd) in fd_copy fd ("this."<>n) ("other."<>n)
                      | fd <- fieldDetails ]
            )

          ctor3g =
            cblock (template "public $1($2 other, $3)" [d_name decl, d_name decl <> typeArgs, classArgs]) (
              cblock "try" ( 
                mconcat [ let n = f_name (fd_field fd) in fd_copy fd ("this."<>n) ("other."<>n)
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
    

