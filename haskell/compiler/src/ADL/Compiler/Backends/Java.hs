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
  cgp_mutable :: Bool
}

data ClassFile = ClassFile {
   cf_module :: ModuleName,
   cf_decl :: T.Text,
   cf_fields :: [T.Text],
   cf_methods :: [[T.Text]]
}

classFile :: ModuleName -> T.Text -> ClassFile
classFile mname decl = ClassFile mname decl [] []

type CState a = State ClassFile a

instance MGen (State ClassFile) where
  getPrimitiveType = pd_unboxed . genPrimitiveDetails 
  getPrimitiveDefault pt = return (pd_default (genPrimitiveDetails pt))
  getPrimitiveLiteral pt jv = return (pd_genLiteral (genPrimitiveDetails pt) jv)
  getTypeExpr _ te = genTypeExpr te
  getTypeExprB _ _ te = genTypeExpr te
  getUnionConstructorName d f = return "FIXME"

addField :: T.Text -> CState ()
addField declString = modify (\cf->cf{cf_fields=declString:cf_fields cf})

addMethod :: [T.Text] -> CState ()
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
  pd_copy :: T.Text -> T.Text
}

numPrimitive :: T.Text -> T.Text -> PrimitiveDetails
numPrimitive unboxed boxed = PrimitiveDetails {
  pd_unboxed = return unboxed,
  pd_boxed = return boxed,
  pd_default = Just "0",
  pd_genLiteral = \(JSON.Number n) -> litNumber n,
  pd_copy = id
  }
  
genPrimitiveDetails :: PrimitiveType -> PrimitiveDetails
genPrimitiveDetails P_Void = PrimitiveDetails {
  pd_unboxed = return "Void",
  pd_boxed = return "Void",
  pd_default = Just "null",
  pd_genLiteral = \jv -> "null",
  pd_copy = id
  }
genPrimitiveDetails P_Bool = PrimitiveDetails {
  pd_unboxed = return "boolean",
  pd_boxed = return "Boolean",
  pd_default = Just "false",
  pd_genLiteral = \jv ->
    case jv of
      (JSON.Bool True) -> "true"
      (JSON.Bool False) -> "false",
  pd_copy = id
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
  pd_copy = \v -> template "java.util.Arrays.copyOf($1, $1.length)" [v]
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
  pd_copy = \v -> template "new java.util.ArrayList($1)" [v] -- FIXME should be a deep copy
  }
genPrimitiveDetails P_String = PrimitiveDetails {
  pd_unboxed = return "String",
  pd_boxed = return "String",
  pd_default = Just "\"\"",
  pd_genLiteral = \(JSON.String s) -> T.pack (show s),
  pd_copy = id
  }
genPrimitiveDetails P_Sink = PrimitiveDetails {
  pd_unboxed = return "Sink",
  pd_boxed = return "Sink",
  pd_default = Just "new Sink()",
  pd_genLiteral = \_ -> "????", -- never called
  pd_copy = \v -> template "new Sink($1)" [v]
  }

indent ::T.Text
indent = "  "

data FieldDetails = FieldDetails {
  fd_field :: Field ResolvedType,
  fd_typeExprStr :: T.Text,
  fd_defValue :: Literal,
  fd_copy :: T.Text -> T.Text
}

genFieldDetails :: Field ResolvedType -> CState FieldDetails
genFieldDetails f = do
  typeExprStr <- genTypeExpr (f_type f)
  litv <- case f_default f of
    (Just v) -> mkLiteral (f_type f) v
    Nothing -> mkDefaultLiteral (f_type f)
  let copy = case (f_type f) of
        (TypeExpr (RT_Primitive pt) []) -> pd_copy (genPrimitiveDetails pt)
        _ -> \v -> template "new $1($2)" [typeExprStr,v]
  return (FieldDetails f typeExprStr litv copy)

generateModule :: (ModuleName -> JavaPackage) ->
                  (ScopedName -> FilePath) ->
                  (ScopedName -> CodeGenProfile) ->
                  (FilePath -> LBS.ByteString -> IO ()) ->
                  Module ResolvedType ->
                  EIO a ()
generateModule mPackage mFile mCodeGetProfile fileWriter m = do
  let decls = Map.elems (m_decls m)
  for_ decls $ generateDecl
  where
    generateDecl decl = case d_type decl of
      Decl_Struct s -> writeClassFile (declFile decl) (generateStruct decl s)
      _ -> return ()

    declFile decl = mFile (ScopedName (m_name m) (d_name decl))

    writeClassFile :: FilePath -> ClassFile -> EIO a ()
    writeClassFile path content = do
      let lines = [ template "package $1;" [genJavaPackage (mPackage (cf_module content))]
                  , ""
                  , template "$1 {" [cf_decl content]
                  ] <> 
                  [ indent <> fieldStr <> ";" | fieldStr <- reverse (cf_fields content)
                  ] <>
                  [ ""
                  ] <>
                  [ T.intercalate "\n" (map (indent <>) method) <> "\n" | method <- reverse (cf_methods content)
                  ] <>
                  [ "}"
                  ]
      liftIO $ fileWriter path (LBS.fromStrict (T.encodeUtf8 (T.intercalate "\n" lines)))
  
    generateStruct decl struct =  execState gen state0
      where
        state0 = classFile (m_name m) classDecl
        classDecl = "public class " <> d_name decl <> typeArgs
        typeArgs = case s_typeParams struct of
          [] -> ""
          args -> "<" <> T.intercalate "," args <> ">"
        gen = do
          for_ (s_fields struct) $ \field -> do
            typeExpr <- genTypeExpr (f_type field)
            addField ("public " <> typeExpr <> " " <> f_name field)
          fieldDetails <- mapM genFieldDetails (s_fields struct)
          let ctorArgs =  T.intercalate ", " [fd_typeExprStr fd <> " " <> f_name (fd_field fd) | fd <- fieldDetails]
              ctor1 =
               [ template "public $1($2) {" [d_name decl,ctorArgs]
               ] <>
               [ indent <> template "this.$1 = $1;" [f_name f] | f <- s_fields struct
               ] <>
               [ "}"
               ]

              ctor2 =
               [ template "public $1($2 other) {" [d_name decl, d_name decl <> typeArgs]
               ] <>
               [ indent <> template "this.$1 = $2;" (let n = f_name (fd_field fd) in [n,fd_copy fd ("other." <> n)])
                 | fd <- fieldDetails
               ] <>
               [ "}"
               ]

              ctor3 =
               [ template "public $1() {" [d_name decl]
               ] <>
               [ indent <> template "this.$1 = $2;" [f_name (fd_field fd),literalValue (fd_defValue fd)] | fd <- fieldDetails
               ] <>
               [ "}"
               ]

          addMethod ctor1 
          addMethod ctor2
          addMethod ctor3

literalValue :: Literal -> T.Text
literalValue (LDefault t) = template "new $1()" [t]
literalValue (LCtor t ls) = template "new $1($2)" [t, T.intercalate ", " (map literalValue ls)]
literalValue (LUnion t ctor l) = template "$1.$2($3)" [t, ctor, literalValue l ]
literalValue (LVector t ls) = template "java.util.Arrays.asList($1)" [T.intercalate "," (map literalValue ls)] -- FIXME
literalValue (LPrimitive _ t) = t
          

packageGenerator :: T.Text -> ModuleName -> JavaPackage
packageGenerator basePackage mn = JavaPackage (T.splitOn "." basePackage <> unModuleName mn)

fileGenerator :: T.Text -> ScopedName -> FilePath
fileGenerator basePackage sn = T.unpack (T.intercalate "/" idents <> ".java")
  where
    idents = unJavaPackage (packageGenerator basePackage (sn_moduleName sn)) <> [sn_name sn]

defaultCodeGenProfile = CodeGenProfile False

generate :: JavaFlags -> [FilePath] -> EIOT ()
generate jf modulePaths = catchAllExceptions  $ for_ modulePaths $ \modulePath -> do
  m <- loadAndCheckModule (moduleFinder (jf_searchPath jf)) modulePath
  generateModule (packageGenerator (jf_package jf))
                 (fileGenerator (jf_package jf))
                 (const defaultCodeGenProfile)
                 (jf_fileWriter jf)
                 m


