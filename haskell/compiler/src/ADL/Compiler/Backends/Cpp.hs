{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Cpp(
  generate,
  CppFlags(..)
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Monoid
import Data.Ord (comparing)

import System.FilePath(joinPath,takeDirectory,(</>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS

import qualified Text.Parsec as P
import qualified ADL.Compiler.ParserP as P

import ADL.Utils.Format
import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Processing
import qualified ADL.Compiler.Processing as PL
import ADL.Compiler.Primitive
import ADL.Compiler.Utils
import ADL.Core.Value

import qualified ADL.Adlc.Config.Cpp as CC

newtype CppNamespace = CppNamespace { unCppNamespace :: [Ident] }
   deriving (Eq)

newtype CppFilePath = CppFilePath FilePath

data IncFilePath = IncFilePath FilePath Bool deriving (Eq,Ord)

instance Format CppNamespace where formatText (CppNamespace ids) = T.intercalate "::" ids

instance Format IncFilePath where
  format (IncFilePath fp False) = "<" ++ fp ++ ">"
  format (IncFilePath fp True) = "\"" ++ fp ++ "\""

instance Format CppFilePath where format (CppFilePath fp) = fp

-- The state of a file section for which we are accumulating
-- content
data FState = FState {
  fs_includes :: Set.Set IncFilePath,
  fs_lines :: [T.Text],
  fs_namespace :: CppNamespace
  }

-- Produce the actual text form of the accumulated file states
fileLines ::[FState] -> [T.Text]
fileLines fss = includesT ++ concat (map bodyT fss)
  where
    includesT = [ template "#include $1" [formatText i] | i <- Set.toList includes ]
    includes = Set.unions (map fs_includes fss)
    bodyT fs = inNamespace (fs_namespace fs) (reverse (fs_lines fs))

    inNamespace :: CppNamespace -> [T.Text] -> [T.Text]
    inNamespace (CppNamespace ids) [] = []
    inNamespace (CppNamespace ids) lines
      =  [""]
      ++ [template "namespace $1 {" [i] | i <- ids ]
      ++ lines
      ++ [""]
      ++ [template "$1; // $2" [T.replicate (length ids) "}",T.intercalate "::" ids ]]

-- | The state capturing all the output being generated.
data MState = MState {
   ms_name :: ModuleName,
   ms_moduleMapper :: ModuleName -> CppNamespace,
   ms_incFileMapper :: ModuleName -> FilePath,
   ms_incFileUserModule :: FState,
   ms_incFileSerialisation :: FState,
   ms_cppFileUserModule :: FState,
   ms_cppFileSerialisation :: FState
}

data CustomType = CustomType {
   ct_name :: Ident,
   ct_includes :: Set.Set IncFilePath,
   ct_generateOrigADLType :: Maybe Ident,
   ct_declarationCode :: [T.Text],
   ct_serialisationCode :: [T.Text]
}

-- A variant of the AST that carries custom type
-- information.

type CResolvedType = ResolvedTypeT (Maybe CustomType)
type CModule = Module (Maybe CustomType) CResolvedType
type CTypeExpr = TypeExpr CResolvedType
type CDecl = Decl (Maybe CustomType) CResolvedType


data BState = BState {
  bs_indent :: T.Text,
  bs_lines :: [T.Text]
  }

type CodeWriter a = State BState a

wl :: T.Text -> CodeWriter ()
wl t = modify (\bs->bs{bs_lines=T.append (bs_indent bs) t:bs_lines bs})

wt :: T.Text -> [T.Text] -> CodeWriter ()
wt t ps = wl (template t ps)

indent :: CodeWriter a -> CodeWriter a
indent cw = do
  i <- gets bs_indent
  modify (\bs->bs{bs_indent=T.append i "    "})
  a <- cw
  modify (\bs->bs{bs_indent=i})
  return a

cblock :: CodeWriter a -> CodeWriter a
cblock code = do
  wl "{"
  a <- indent code
  wl "}"
  return a

dblock :: CodeWriter a -> CodeWriter a
dblock code = do
  wl "{"
  a <- indent code
  wl "};"
  return a

write :: FileRef -> CodeWriter a -> Gen a
write fr cw = do
    modify (fr (\fs->fs{fs_lines=bs_lines bs++fs_lines fs}))
    return a
  where
    (a,bs) = runState cw (BState "" [])

type Gen = State MState

-- Selector function to control which file is being updated.
type FileRef = (FState -> FState) -> MState-> MState

ifile, ifileS, cppfile, cppfileS :: FileRef
ifile fu ms = ms{ms_incFileUserModule=fu (ms_incFileUserModule ms)}
ifileS fu ms = ms{ms_incFileSerialisation=fu (ms_incFileSerialisation ms)}
cppfile fu ms = ms{ms_cppFileUserModule=fu (ms_cppFileUserModule ms)}
cppfileS fu ms = ms{ms_cppFileSerialisation=fu (ms_cppFileSerialisation ms)}


-- Reference an include file from the given file
include, includeStd :: FileRef -> FilePath -> Gen ()
include fr i = include0 fr (IncFilePath i True)
includeStd fr i = include0 fr (IncFilePath i False)

include0 :: FileRef -> IncFilePath -> Gen ()
include0 fl i = modify (fl $ \fs -> fs{fs_includes=Set.insert i (fs_includes fs)})

type TypeBindingMap = Map.Map Ident (TypeExpr CResolvedType)

-- Returns the c++ typer expression corresponding to the
-- given ADL type expression
cTypeExpr :: Bool -> TypeExpr CResolvedType -> Gen T.Text
cTypeExpr scopeLocalNames te = cTypeExprB scopeLocalNames Map.empty te

cTypeExprB :: Bool -> TypeBindingMap -> TypeExpr CResolvedType -> Gen T.Text
cTypeExprB scopeLocalNames m (TypeExpr c args) = do
  targs <- mapM (cTypeExprB scopeLocalNames m) args
  cTypeExprB1 scopeLocalNames m c targs

cTypeExprB1 :: Bool -> TypeBindingMap -> CResolvedType -> [T.Text] -> Gen T.Text
cTypeExprB1 scopeLocalNames _ (RT_Named (sn,decl)) targs = do
  ms <- get
  let isLocalName = case sn_moduleName sn of
        ModuleName [] -> True
        _ -> False
      fullyScopedName = if isLocalName then sn{sn_moduleName=ms_name ms} else sn

  ctype <- case d_customType decl of
    (Just ct) -> do
      -- custom type
      mapM_ (include0 ifile) (Set.toList $ ct_includes ct)
      return (ct_name ct)
    Nothing -> case isLocalName of
      True ->
        -- ADL type defined in this module
        if scopeLocalNames
          then do
            let ns = ms_moduleMapper ms (ms_name ms)
            return (template "$1::$2" [formatText ns, cTypeName (sn_name sn)])
          else return (cTypeName (sn_name sn))

      False -> do
        -- ADL type defined in an imported module
        let m = sn_moduleName sn
            namespace = ms_moduleMapper ms m
        includeModule ifile m
        return (template "$1::$2" [formatText namespace, cTypeName (sn_name sn)])
  return (withTypeParams ctype targs)

cTypeExprB1 scopeLocalNames m (RT_Param i) targs = case Map.lookup i m of
    (Just te) -> do
      e <- cTypeExprB scopeLocalNames m te
      return (withTypeParams e targs)
    Nothing -> return (withTypeParams (cTypeParamName i) targs)
cTypeExprB1 scopeLocalNames _ (RT_Primitive pt) targs = cPrimitiveType pt targs

withTypeParams :: T.Text -> [T.Text] -> T.Text
withTypeParams e [] = e
withTypeParams e targs = template "$1<$2> " [e,T.intercalate "," targs]

reservedWords = Set.fromList
  [ "null"
  , "alignas"
  , "alignof"
  , "and"
  , "and_eq"
  , "asm"
  , "auto"
  , "bitand"
  , "bitor"
  , "bool"
  , "break"
  , "case"
  , "catch"
  , "char"
  , "char16_t"
  , "char32_t"
  , "class"
  , "compl"
  , "const"
  , "constexpr"
  , "const_cast"
  , "continue"
  , "decltype"
  , "default"
  , "delete"
  , "do"
  , "double"
  , "dynamic_cast"
  , "else"
  , "enum"
  , "explicit"
  , "export"
  , "extern"
  , "false"
  , "float"
  , "for"
  , "friend"
  , "goto"
  , "if"
  , "inline"
  , "int"
  , "long"
  , "mutable"
  , "namespace"
  , "new"
  , "noexcept"
  , "not"
  , "not_eq"
  , "nullptr"
  , "operator"
  , "or"
  , "or_eq"
  , "private"
  , "protected"
  , "public"
  , "register"
  , "reinterpret_cast"
  , "return"
  , "short"
  , "signed"
  , "sizeof"
  , "static"
  , "static_assert"
  , "static_cast"
  , "struct"
  , "switch"
  , "template"
  , "this"
  , "thread_local"
  , "throw"
  , "true"
  , "try"
  , "typedef"
  , "typeid"
  , "typename"
  , "union"
  , "unsigned"
  , "using"
  , "virtual"
  , "void"
  , "volatile"
  , "wchar_t"
  , "while"
  , "xor"
  , "xor_eq                           "
  ]

unreserveWord :: Ident -> Ident
unreserveWord n | Set.member (T.toLower n) reservedWords = T.append n "_"
                | otherwise = n

-- Returns the c++ name corresponding to the ADL type name
cTypeName :: Ident -> Ident
cTypeName = unreserveWord

-- Returns the c++ name corresponding to the ADL type parameter
cTypeParamName :: Ident -> Ident
cTypeParamName = unreserveWord

-- Returns the c++ name corresponding to the ADL field name
cFieldName :: Ident -> Ident
cFieldName = unreserveWord

-- Returns the c++ name for the accessor function for a union field
cUnionAccessorName :: Ident  -> Ident
cUnionAccessorName n = unreserveWord n

-- Returns the c++ name for the constructor function for a union field
cUnionConstructorName :: Ident -> Ident
cUnionConstructorName n = T.append "mk_" (cUnionAccessorName n)

-- Returns the c++ name for the setter function for a union field
cUnionSetterName :: Ident -> Ident
cUnionSetterName n = T.append "set_" (cUnionAccessorName n)

-- Returns the c++ name for the enum value corresponding to the ADL discriminator name
cUnionDiscName :: Ident -> Ident
cUnionDiscName n = T.toUpper (cUnionAccessorName n)

-- Return an expression to construct a serialiser for a given type
cSerializerExpr :: TypeExpr CResolvedType -> Gen T.Text
cSerializerExpr (TypeExpr (RT_Primitive P_StringMap) [te]) = do
  t <- cTypeExpr True te
  return (template "stringMapSerialiser<$1>" [t])
cSerializerExpr (TypeExpr (RT_Primitive P_Nullable) [te]) = do
  t <- cTypeExpr True te
  return (template "nullableSerialiser<$1>" [t])
cSerializerExpr te = do
  t <- cTypeExpr True te
  return (template "Serialisable<$1>::serialiser" [t])

includeModule :: FileRef -> ModuleName -> Gen ()
includeModule fr mn = do
  ms <- get
  let fp2 = ms_incFileMapper ms mn
  include fr (fp2 ++ ".h")

genTemplate :: [Ident] -> CodeWriter ()
genTemplate [] = return ()
genTemplate tps = wt "template <$1>"
                  [T.intercalate ", " [T.concat ["class ",cTypeParamName tp] | tp <- tps]]

genTemplateI :: [Ident] -> CodeWriter ()
genTemplateI [] = wl "inline"
genTemplateI tps = genTemplate tps

addMarker :: v -> v -> v -> [a] -> [(v,a)]
addMarker fv v lv as = case add as of
    [] -> []
    ((_,a):as) -> (fv,a):as
  where
    add [] = []
    add [a] = [(lv,a)]
    add (a:as) = (v,a):add as

sepWithTerm :: T.Text -> T.Text -> [v] -> [(v,T.Text)]
sepWithTerm _ _ [] = []
sepWithTerm _ term [v] = [(v,term)]
sepWithTerm sep term (v:vs) = (v,sep):sepWithTerm sep term vs

commaSep :: [v]  -> [(v,T.Text)]
commaSep = sepWithTerm "," ""

declareOperators fr tparams ctnameP = do
  wl ""
  genTemplate tparams
  wt "bool operator<( const $1 &a, const $1 &b );" [ctnameP]
  genTemplate tparams
  wt "bool operator==( const $1 &a, const $1 &b );" [ctnameP]

declareSerialisation tparams ms ctnameP = do
  let ns = ms_moduleMapper ms (ms_name ms)
      ctnameP1 = template "$1::$2" [formatText ns,ctnameP]
  wl ""
  case tparams of
    [] -> do
      wl "template <>"
      wt "struct Serialisable<$1>" [ctnameP1]
      dblock $ wt "static Serialiser<$1>::Ptr serialiser(const SerialiserFlags &);" [ctnameP1]
    _ -> do
      genTemplate tparams
      wt "struct Serialisable<$1>" [ctnameP1]
      dblock $ wt "static typename Serialiser<$1>::Ptr serialiser(const SerialiserFlags &);" [ctnameP1]

data FieldDetails = FieldDetails {
  fd_field :: Field CResolvedType,
  fd_fieldName :: Ident,
  fd_typeExpr :: T.Text,
  fd_scopedTypeExpr :: T.Text,
  fd_defValue :: Literal CTypeExpr,
  fd_defLValue :: T.Text,
  fd_defPValue :: T.Text,
  fd_isVoidType :: Bool,
  fd_unionDiscName :: T.Text,
  fd_unionAccessorName :: T.Text,
  fd_unionSetterName :: T.Text,
  fd_unionConstructorName :: T.Text,
  fd_serializedName :: Ident,
  fd_serializerExpr :: T.Text
  }

genFieldDetails :: Field CResolvedType -> Gen FieldDetails
genFieldDetails f = do
  let te = f_type f
  t <- cTypeExpr False te
  scopedt <- cTypeExpr True te
  defValue <- case f_default f of
    (Just v) -> case literalForTypeExpr te v of
      Left e -> error ("BUG: invalid json literal: " ++ T.unpack e)
      Right litv -> return litv
    Nothing -> return (Literal te LDefault)
  defLValue <- literalLValue defValue
  defPValue <- literalPValue defValue
  serializerExpr <- cSerializerExpr te
  return $ FieldDetails {
    fd_field=f,
    fd_fieldName=cFieldName (f_name f),
    fd_typeExpr=t,
    fd_scopedTypeExpr=scopedt,
    fd_defValue=defValue,
    fd_defLValue=defLValue,
    fd_defPValue=defPValue,
    fd_isVoidType=isVoidType te,
    fd_unionDiscName=cUnionDiscName (f_name f),
    fd_unionAccessorName=cUnionAccessorName (f_name f),
    fd_unionSetterName=cUnionSetterName (f_name f),
    fd_unionConstructorName=cUnionConstructorName (f_name f),
    fd_serializedName=f_serializedName f,
    fd_serializerExpr=serializerExpr
    }

generateFwdDecl1 :: Ident -> [Ident] -> Gen ()
generateFwdDecl1 dn tparams = do
  write ifile $ do
    wl ""
    genTemplate tparams
    wt "struct $1;" [cTypeName dn]

generateFwdDecl :: Ident -> CDecl -> Gen ()
generateFwdDecl dn (Decl{d_type=(Decl_Struct s)}) = generateFwdDecl1 dn (s_typeParams s)
generateFwdDecl dn (Decl{d_type=(Decl_Union u)}) = generateFwdDecl1 dn (u_typeParams u)
generateFwdDecl dn (Decl{d_type=(Decl_Newtype n)}) = generateFwdDecl1 dn (n_typeParams n)
generateFwdDecl _ (Decl{d_type=(Decl_Typedef _)}) = error "BUG: Unexpected fwd declaration of typedef"

generateDecl :: Ident -> CDecl -> Gen ()
generateDecl dn d@(Decl{d_type=(Decl_Struct s)}) = do
  ms <- get
  fds <- mapM genFieldDetails (s_fields s)

  let isEmpty = null (s_fields s)
      ns = ms_moduleMapper ms (ms_name ms)
      ctname = cTypeName dn
      ctnameP = case s_typeParams s of
        [] -> ctname
        ids -> template "$1<$2>" [ctname,T.intercalate "," ids]

      -- icfile is where we write definitions: the include file if
      -- parametrised, otherwise the cpp file
      icfile = if null (s_typeParams s) then cppfile else ifile
      icfileS = if null (s_typeParams s) then cppfileS else ifileS

  -- Class Declaration
  write ifile $ do
    wl ""
    genTemplate (s_typeParams s)
    wt "struct $1" [ctname]
    dblock $ do
       wt "$1();" [ctname]
       wl ""
       when (not isEmpty) $ do
         wt "$1(" [ctname]
         indent $ do
           forM_ (commaSep fds) $ \(fd,sep) -> do
              wt "const $1 & $2$3" [fd_typeExpr fd,fd_fieldName fd,sep]
           wl ");"
         wl ""
       forM_ fds $ \fd -> do
           wt "$1 $2;" [fd_typeExpr fd, fd_fieldName fd]

    declareOperators ifile (s_typeParams s) ctnameP

  write icfile $ do
    -- Constructors
    wl ""
    genTemplate (s_typeParams s)
    wt "$1::$2()" [ctnameP, ctname]
    indent $ do
      let ifds = filter (literalNeedsInit . fd_defValue) fds
      forM_ (addMarker ":" "," "," ifds) $ \(mark,fd) -> do
          wt "$1 $2($3)" [mark,fd_fieldName fd,fd_defLValue fd]
    cblock $ return ()
    when (not isEmpty) $ do
      wl ""
      genTemplate (s_typeParams s)
      wt "$1::$2(" [ctnameP,ctname]
      indent $ do
        forM_ (commaSep fds) $ \(fd,sep) -> do
          wt "const $1 & $2_$3" [fd_typeExpr fd,fd_fieldName fd,sep]
        wl ")"
        forM_ (addMarker ":" "," "," fds) $ \(mark,fd) -> do
          wt "$1 $2($2_)" [mark,fd_fieldName fd]
      cblock $ return ()

    -- Non-inline functions
    wl ""
    genTemplate (s_typeParams s)
    wl "bool"
    wt "operator<( const $1 &a, const $1 &b )" [ctnameP]
    cblock $ do
      forM_ fds $ \fd -> do
        wt "if( a.$1 < b.$1 ) return true;" [fd_fieldName fd]
        wt "if( b.$1 < a.$1 ) return false;" [fd_fieldName fd]
      wl "return false;"
    wl ""
    genTemplate (s_typeParams s)
    wl "bool"
    wt "operator==( const $1 &a, const $1 &b )" [ctnameP]
    cblock $ do
      if isEmpty
        then wl "return true;"
        else do
          wl "return"
          forM_ (sepWithTerm "&&" ";" fds) $ \(fd,sep) -> do
            indent $ wt "a.$1 == b.$1 $2" [fd_fieldName fd,sep]

  write ifileS $ do
    declareSerialisation (s_typeParams s) ms ctnameP

  write icfileS $ do
    wl ""
    genTemplate (s_typeParams s)
    wt "typename Serialiser<$1::$2>::Ptr" [formatText ns,ctnameP]
    wt "Serialisable<$1::$2>::serialiser( const SerialiserFlags &sf )" [formatText ns,ctnameP]
    dblock $ do
        wt "typedef $1::$2 _T;" [formatText ns,ctnameP]
        wl ""
        wl "struct S_ : public Serialiser<_T>"
        dblock $ do
            wl "S_( const SerialiserFlags & sf )"
            indent $ do
              forM_ (addMarker ":" "," "," fds) $ \(mark,fd) -> do
                wt "$1 $2_s( $3(sf) )" [mark,fd_fieldName fd,fd_serializerExpr fd]
              wl "{}"
            wl ""
            wl ""
            forM_ fds $ \fd -> do
              wt "typename Serialiser<$1>::Ptr $2_s;" [fd_scopedTypeExpr fd,fd_fieldName fd]
            wl ""
            wl "void toJson( JsonWriter &json, const _T & v ) const"
            cblock $ do
              wl "json.startObject();"
              forM_ fds $ \fd -> do
                wt "writeField<$1>( json, $3_s, \"$2\", v.$3 );" [fd_scopedTypeExpr fd,fd_serializedName fd,fd_fieldName fd]
              wl "json.endObject();"
              return ()
            wl ""
            wl "void fromJson( _T &v, JsonReader &json ) const"
            cblock $ do
              wl "match( json, JsonReader::START_OBJECT );"
              wl "while( !match0( json, JsonReader::END_OBJECT ) )"
              cblock $ do
                forM_ fds $ \fd -> do
                  wt "readField( $1_s, v.$1, \"$2\", json ) ||" [fd_fieldName fd,fd_serializedName fd]
                wl "ignoreField( json );"
        wl ""
        wl "return typename Serialiser<_T>::Ptr( new S_(sf) );"

generateDecl dn d@(Decl{d_type=(Decl_Union u)}) = do
  ms <- get
  fds <- mapM genFieldDetails (u_fields u)

  let ns = ms_moduleMapper ms (ms_name ms)
      ctname = cTypeName dn
      ctnameP = case u_typeParams u of
        [] -> ctname
        ids -> template "$1<$2>" [ctname,T.intercalate "," ids]

      -- icfile is where we write definitions: the include file if
      -- parametrised, otherwise the cpp file
      icfile = if null (u_typeParams u) then cppfile else ifile
      icfileS = if null (u_typeParams u) then cppfileS else ifileS

  write ifile $ do
    -- The class declaration
    wl ""
    genTemplate (u_typeParams u)
    wt "class $1" [ctname]
    wl "{"
    wl "public:"
    indent $ do
      wt "$1();" [ctname]
      forM_ fds $ \fd -> do
        let ctorName = fd_unionConstructorName fd
        if fd_isVoidType fd
          then wt "static $1 $2();" [ctnameP, ctorName]
          else wt "static $1 $2( const $3 & v );" [ctnameP, ctorName, fd_typeExpr fd]

      wl ""
      wt "$1( const $2 & );" [ctname,ctnameP]
      wt "~$1();" [ctname]
      wt "$1 & operator=( const $1 & );" [ctnameP]
      wl ""
      wl "enum DiscType"
      dblock $
        forM_ (commaSep fds) $ \(fd,sep) -> do
          wt "$1$2" [fd_unionDiscName fd, sep]
      wl ""
      wl "DiscType d() const;"
      forM_ fds $ \fd -> do
        when (not $ fd_isVoidType fd) $
          wt "$1 & $2() const;" [(fd_typeExpr fd),fd_unionAccessorName fd]
      wl ""
      forM_ fds $ \fd -> do
        if fd_isVoidType fd
          then wt "void $1();" [fd_unionSetterName fd]
          else wt "const $1 & $2(const $1 & );" [(fd_typeExpr fd),fd_unionSetterName fd]
      wl ""
    wl "private:"
    indent $ do
      wt "$1( DiscType d, void * v);" [ctname]
      wl ""
      wl "DiscType d_;"
      wl "void *p_;"
      wl ""
      wl "static void free( DiscType d, void *v );"
      wt "static void *copy( DiscType d, void *v );" [ctnameP]
    wl "};"

    declareOperators ifile (u_typeParams u) ctnameP

  write ifile $ do
    wl ""
    genTemplate (u_typeParams u)
    case u_typeParams u of
      [] ->  wt "inline $1::DiscType $1::d() const" [ctnameP]
      _ ->  wt "typename $1::DiscType $1::d() const" [ctnameP]
    cblock $ do
      wl "return d_;"

    forM_ fds $ \fd -> do
      when (not $ fd_isVoidType fd) $ do
        wl ""
        genTemplate (u_typeParams u)
        wt "inline $1 & $2::$3() const" [fd_typeExpr fd,ctnameP,fd_unionAccessorName fd]
        cblock $ do
          wt "if( d_ == $1 )" [fd_unionDiscName fd]
          cblock $ do
            wt "return *($1 *)p_;" [fd_typeExpr fd]
          wl "throw invalid_union_access();"

  write icfile $ do
    wl ""
    genTemplate (u_typeParams u)
    wt "$1::$2()" [ctnameP,ctname]
    -- FIXME :: Confirm that typechecker disallows empty unions, so the
    -- head below is ok.
    let fd = head fds
        lv = if fd_isVoidType fd
             then "0"
             else template "new $1" [fd_defPValue fd]
    indent $ wt ": d_($1), p_($2)" [fd_unionDiscName fd,lv]
    wl "{"
    wl "}"
    forM_ fds $ \fd -> do
      let ctorName = fd_unionConstructorName fd
      wl ""
      genTemplate (u_typeParams u)
      if fd_isVoidType fd
        then do
          wt "$1 $1::$2()" [ctnameP, ctorName ]
          cblock $
            wt "return $1( $2, 0 );" [ctnameP, fd_unionDiscName fd]
        else do
          wt "$1 $1::$2( const $3 & v )" [ctnameP, ctorName,fd_typeExpr fd]
          cblock $
            wt "return $1( $2, new $3(v) );" [ctnameP, fd_unionDiscName fd,fd_typeExpr fd]
    wl ""
    genTemplate (u_typeParams u)
    wt "$1::$2( const $1 & v )" [ctnameP,ctname]
    indent $ wl ": d_(v.d_), p_(copy(v.d_,v.p_))"
    cblock $ return ()
    wl ""
    genTemplate (u_typeParams u)
    wt "$1::~$2()" [ctnameP,ctname]
    cblock $ do
      wl "free(d_,p_);"
    wl ""
    genTemplate (u_typeParams u)
    wt "$1 & $1::operator=( const $1 & o )" [ctnameP]
    cblock $ do
      wl "free(d_,p_);"
      wl "d_ = o.d_;"
      wl "p_ = copy( o.d_, o.p_ );"
      wl "return *this;"

    forM_ fds $ \fd -> do
      wl ""
      genTemplate (u_typeParams u)
      if fd_isVoidType fd
        then do
          wt "void $1::$2()" [ctnameP,fd_unionSetterName fd]
          cblock $ do
            wt "if( d_ != $1 )" [fd_unionDiscName fd]
            cblock $ do
              wl "free(d_,p_);"
              wt "d_ = $1;" [fd_unionDiscName fd]
              wt "p_ = 0;" [fd_unionDiscName fd]

        else do
          wt "const $1 & $2::$3(const $1 &v)" [fd_typeExpr fd,ctnameP,fd_unionSetterName fd]
          cblock $ do
            wt "if( d_ == $1 )" [fd_unionDiscName fd]
            cblock $ do
              wt "*($1 *)p_ = v;" [fd_typeExpr fd]
            wl "else"
            cblock $ do
              wl "free(d_,p_);"
              wt "d_ = $1;" [fd_unionDiscName fd]
              wt "p_ = new $1(v);" [fd_typeExpr fd]
            wt "return *($1 *)p_;" [fd_typeExpr fd]

    wl ""
    genTemplate (u_typeParams u)
    wt "$1::$2(DiscType d, void *p)" [ctnameP,ctname]
    indent $ wl ": d_(d), p_(p)"
    cblock $ return ()

    wl ""
    genTemplate (u_typeParams u)
    wt "void $1::free(DiscType d, void *p)" [ctnameP]
    cblock $ do
      wl "switch( d )"
      cblock $
        forM_ fds $ \fd -> do
          if fd_isVoidType fd
            then wt "case $1: return;"
                 [fd_unionDiscName fd]
            else wt "case $1: delete ($2 *)p; return;"
                 [fd_unionDiscName fd,fd_typeExpr fd]
    wl ""
    genTemplate (u_typeParams u)
    wt "void * $1::copy( DiscType d, void *p )" [ctnameP]
    cblock $ do
      wl "switch( d )"
      cblock $
        forM_ fds $ \fd -> do
          if fd_isVoidType fd
            then wt "case $1: return 0;"
                 [fd_unionDiscName fd]
            else wt "case $1: return new $2(*($2 *)p);"
                 [fd_unionDiscName fd,fd_typeExpr fd]
      wl "return 0;"
    wl ""
    genTemplate (u_typeParams u)
    wl "bool"
    wt "operator<( const $1 &a, const $1 &b )" [ctnameP]
    cblock $ do
      wl "if( a.d() < b.d() ) return true;"
      wl "if( b.d() < a.d()) return false;"
      wl "switch( a.d() )"
      cblock $
        forM_ fds $ \fd -> do
          if fd_isVoidType fd
            then wt "case $1::$2: return false;"
                 [ctnameP,fd_unionDiscName fd]
            else wt "case $1::$2: return a.$3() < b.$3();"
                 [ctnameP,fd_unionDiscName fd,fd_unionAccessorName fd]
      wl "return false;"

    wl ""
    genTemplate (u_typeParams u)
    wl "bool"
    wt "operator==( const $1 &a, const $1 &b )" [ctnameP]
    cblock $ do
      wl "if( a.d() != b.d() ) return false;"
      wl "switch( a.d() )"
      cblock $
        forM_ fds $ \fd -> do
          if fd_isVoidType fd
            then wt "case $1::$2: return true;"
                 [ctnameP,fd_unionDiscName fd]
            else wt "case $1::$2: return a.$3() == b.$3();"
                 [ctnameP,fd_unionDiscName fd,fd_unionAccessorName fd]
      wl "return false;"

  write ifileS $ do
    declareSerialisation (u_typeParams u) ms ctnameP

  write icfileS $ do
    let ns = ms_moduleMapper ms (ms_name ms)
        scopedctnameP = template "$1::$2" [formatText ns,ctnameP]
    wl ""
    genTemplate (u_typeParams u)
    wt "typename Serialiser<$1>::Ptr" [scopedctnameP]
    wt "Serialisable<$1>::serialiser( const SerialiserFlags &sf )" [scopedctnameP]
    cblock $ do
        wt "typedef $1 _T;" [scopedctnameP]
        wl ""
        wl "struct U_ : public Serialiser<_T>"
        dblock $ do
            wl "U_( const SerialiserFlags & sf )"
            indent $ do
              wl ": sf_(sf)"
              wl "{}"
            wl ""
            wl "SerialiserFlags sf_;";
            forM_ fds $ \fd -> do
              wt "mutable typename Serialiser<$1>::Ptr $2_;" [fd_scopedTypeExpr fd,f_name (fd_field fd)]
            wl ""
            forM_ fds $ \fd -> do
              wt "typename Serialiser<$1>::Ptr $2_s() const" [fd_scopedTypeExpr fd,f_name (fd_field fd)]
              cblock $ do
                wt "if( !$1_ )" [f_name (fd_field fd)]
                indent $ wt "$1_ = $2(sf_);" [f_name (fd_field fd),fd_serializerExpr fd]
                wt "return $1_;" [f_name (fd_field fd)]
              wl ""
            wl "void toJson( JsonWriter &json, const _T & v ) const"
            cblock $ do
              wl "switch( v.d() )"
              cblock $ do
                forM_ fds $ \fd -> do
                  if fd_isVoidType fd
                    then wt "case $1::$2: json.stringV( \"$3\" ); break;" [scopedctnameP,fd_unionDiscName fd,fd_serializedName fd]
                    else wt "case $1::$2: json.startObject(); writeField( json, $3_s(), \"$4\", v.$5() ); json.endObject(); break;"
                           [scopedctnameP,fd_unionDiscName fd, f_name (fd_field fd), fd_serializedName fd,fd_unionAccessorName fd]
              return ()
            wl ""
            let (voidfds,nonvoidfds) = L.partition fd_isVoidType fds
            wl "void fromJson( _T &v, JsonReader &json ) const"
            cblock $ do
              when (not (null voidfds)) $ do
                wl "if( json.type() == JsonReader::STRING )"
                cblock $ do
                  forM_ (addMarker "if" "else if" "else if" voidfds) $ \(ifcmd,fd) -> do
                    wt "$1( json.stringV() == \"$2\" )" [ifcmd,fd_serializedName fd]
                    indent $ wt "v.$1();" [fd_unionSetterName fd]
                  wl "else"
                  indent $ wl "throw json_parse_failure();"
                  wl "json.next();"
                  wl "return;"
              when (not (null nonvoidfds)) $ do
                wl "if( json.type() == JsonReader::START_OBJECT )"
                cblock $ do
                  wl "match( json, JsonReader::START_OBJECT );"
                  wl "if( json.type() == JsonReader::END_OBJECT )"
                  indent $ wl "throw json_parse_failure();"
                  wl "while( !match0( json, JsonReader::END_OBJECT ) )"
                  cblock $ do
                    forM_ (addMarker "if" "else if" "else if" nonvoidfds) $ \(ifcmd,fd) -> do
                      wt "$1( matchField0( \"$2\", json ) )" [ifcmd,fd_serializedName fd]
                      indent $ wt "v.$1($2_s()->fromJson( json ));" [fd_unionSetterName fd,f_name (fd_field fd)]
                    wl "else"
                    indent $ wl "throw json_parse_failure();"
                  wl "return;"
              wl "throw json_parse_failure();"
        wl ""
        wl "return typename Serialiser<_T>::Ptr( new U_(sf) );"

generateDecl dn d@(Decl{d_type=(Decl_Newtype nt)}) = do
  let te = (n_typeExpr nt)
  ms <- get
  t <- cTypeExpr False te
  scopedt <- cTypeExpr True te
  serializer <- cSerializerExpr te
  defValue <- case n_default nt of
    (Just v) -> case literalForTypeExpr te v of
      Left e -> error ("BUG: invalid json literal: " ++ T.unpack e)
      Right litv -> return litv
    Nothing -> return (Literal te LDefault)
  defLValue <- literalLValue defValue

  let ns = ms_moduleMapper ms (ms_name ms)
      tparams = n_typeParams nt
      ctname = cTypeName dn
      ctnameP = case tparams of
        [] -> ctname
        ids -> template "$1<$2>" [ctname,T.intercalate "," ids]
      ctnameP1 = template "$1::$2" [formatText ns,ctnameP]

  write ifile $ do
    wl ""
    genTemplate tparams
    wt "struct $1" [ctname]
    dblock $ do
       if literalNeedsInit defValue
         then wt "$1() : value($2) {}" [ctname,defLValue]
         else wt "$1() {}" [ctname]

       wt "explicit $1(const $2 & v) : value(v) {}" [ctname,t]
       wl ""
       wt "$1 value;" [t]
    wl ""
    genTemplateI tparams
    wt "bool operator<( const $1 &a, const $1 &b ) { return a.value < b.value; }" [ctnameP]
    genTemplateI tparams
    wt "bool operator==( const $1 &a, const $1 &b ) { return a.value == b.value; }" [ctnameP]

  write ifileS $ do
    wl ""
    case tparams of
      [] -> wl "template <>"
      _ -> genTemplate tparams
    wt "struct Serialisable<$1>" [ctnameP1]
    dblock $ do
      wt "struct S : public Serialiser<$1>" [ctnameP1]
      dblock $ do
          wt "S( typename Serialiser<$1>::Ptr s_ ) : s(s_) {}" [scopedt]
          wl ""
          wt "void toJson( JsonWriter &json, const $1 & v ) const" [ctnameP1]
          cblock $ do
            wl "s->toJson( json, v.value );"
          wl ""
          wt "void fromJson( $1 &v, JsonReader &json ) const" [ctnameP1]
          cblock $ do
            wl "s->fromJson( v.value, json );"
          wl ""
          wt "typename Serialiser<$1>::Ptr s;" [scopedt]
      wl ""
      wt "static typename Serialiser<$1>::Ptr serialiser(const SerialiserFlags &sf)" [ctnameP1]
      cblock $ do
        wt "return typename Serialiser<$1>::Ptr(new S($2(sf)));" [ctnameP1,serializer]

generateDecl dn d@(Decl{d_type=(Decl_Typedef t)}) = do
  te <- cTypeExpr False (t_typeExpr t)
  write ifile $ do
    wl ""
    genTemplate (t_typeParams t)
    wt "using $1 = $2;" [cTypeName dn, te]

-- In C++ we either need just the name in scope to use a type (eg for a pointer)
-- or we need the whole type definition (eg to contain a value)
data TypeRefType a = NameOnly a | FullDecl a
   deriving (Eq,Ord,Show)

nameOnly :: TypeRefType a -> TypeRefType a
nameOnly (FullDecl a) = (NameOnly a)
nameOnly t = t

localTypes :: TypeExpr CResolvedType -> Set.Set (TypeRefType Ident)
localTypes (TypeExpr (RT_Primitive P_Vector) [te]) = Set.map nameOnly (localTypes te)
localTypes (TypeExpr c args) = Set.unions (localTypes1 c:[localTypes a | a <- args])

localTypes1 :: CResolvedType -> Set.Set (TypeRefType Ident)
localTypes1 (RT_Named (sn,_)) = case sn_moduleName sn of
    ModuleName [] -> Set.singleton (FullDecl (sn_name sn))
    -- FIXME: need to either check if fully scoped name matches current module here,
    -- or, alternatively, map fully scoped local references to unscoped ones as a compiler
    -- phase.
    _ -> Set.empty
localTypes1 (RT_Param _) = Set.empty
localTypes1 (RT_Primitive _) = Set.empty

referencedLocalTypes :: CDecl -> Set.Set (TypeRefType Ident)
referencedLocalTypes d = Set.difference (rtypes d) selfrefs
  where
    rtypes (Decl{d_type=(Decl_Struct s)}) = Set.unions [ localTypes (f_type f) | f <- s_fields s]
    rtypes (Decl{d_type=(Decl_Union u)}) = Set.map nameOnly (Set.unions [ localTypes (f_type f) | f <- u_fields u])
    rtypes (Decl{d_type=(Decl_Typedef t)}) = localTypes (t_typeExpr t)
    rtypes (Decl{d_type=(Decl_Newtype n)}) = localTypes (n_typeExpr n)

    selfrefs = Set.fromList [NameOnly (d_name d), FullDecl (d_name d)]

ignoreNameOnly :: Ord a => Set.Set (TypeRefType a) -> Set.Set a
ignoreNameOnly ts = Set.fromList [ a | (FullDecl a) <- Set.toList ts]

generateCustomType :: Ident -> CDecl -> CustomType -> Gen ()
generateCustomType n d ct = do
  ms <- get
  mapM_ (include0 ifile) (Set.toList (ct_includes ct))

  write ifile $ wl ""
  case ct_generateOrigADLType ct of
    Nothing -> do
      write ifile $ wt "// $1 has custom definition" [n]
    Just i -> do
      write ifile $ wt "// $1 generated as $2 due to custom definition" [n,i]
      write ifile $ wl ""
      generateDecl i d

  -- Insert the user declaration code
  when (not (null (ct_declarationCode ct))) $ do
    write ifile $ do
      wl ""
      mapM_ wl (ct_declarationCode ct)

  -- Insert the user supplied serialisation code
  when (not (null (ct_serialisationCode ct))) $ do
    write ifileS $ do
      wl ""
      mapM_ wl (ct_serialisationCode ct)

-- |  Expand typedefs present in the definitions of unions and vectors.
--
-- unions and vectors only need forward references, and we make use of this
-- to determine code generation order for mutually recursive types. But we
-- can usefully generate a forward reference to a typedef.
expandUVTypedefs :: CModule -> CModule
expandUVTypedefs m = m{m_decls=decls}
  where
    decls = Map.map (\d->d{d_type=expand1 (d_type d)}) (m_decls m)
    expand1 :: DeclType CResolvedType -> DeclType CResolvedType
    expand1 (Decl_Struct s) = Decl_Struct s{s_fields=map expandSField (s_fields s)}
    expand1 (Decl_Union u) = Decl_Union u{u_fields=map expandUField (u_fields u)}
    expand1 (Decl_Typedef t) = Decl_Typedef t{t_typeExpr=expandVectors (t_typeExpr t)}
    expand1 (Decl_Newtype n) = Decl_Newtype n{n_typeExpr=expandVectors (n_typeExpr n)}

    expandSField :: Field CResolvedType -> Field CResolvedType
    expandSField f = f{f_type=expandVectors (f_type f)}

    expandUField :: Field CResolvedType -> Field CResolvedType
    expandUField f = f{f_type=expandAll (f_type f)}

    -- expand all typedefs present in the given expression
    expandAll :: TypeExpr CResolvedType -> TypeExpr CResolvedType
    expandAll = expandTypedefs

    -- expand only typedefs within Vector<> applications
    expandVectors :: TypeExpr CResolvedType -> TypeExpr CResolvedType
    expandVectors te@(TypeExpr (RT_Primitive P_Vector) _) = expandAll te
    expandVectors (TypeExpr rt tes) = TypeExpr rt (map expandVectors tes)

generateModule :: CModule -> Gen ()
generateModule m0 = do
   ms <- get
   let mname = ms_name ms

       m = expandUVTypedefs m0

       moduleCustomTypes = Set.fromList [n | (n,decl) <- Map.toList (m_decls m), Map.member cppCustomType (d_annotations decl)]

       makeCustomTypeFullRef :: TypeRefType Ident -> TypeRefType Ident
       makeCustomTypeFullRef r@(NameOnly n) = if Set.member n moduleCustomTypes then (FullDecl n) else r
       makeCustomTypeFullRef r = r

       referencedLocalTypes' = Set.map makeCustomTypeFullRef . referencedLocalTypes

       -- generate the types for which we want forward references
       allRefs = (Set.unions . map referencedLocalTypes' . Map.elems . m_decls) m
       fwdRefs = [ n | (NameOnly n) <- Set.toList allRefs]

       genFwdDecl n = case Map.lookup n (m_decls m) of
           (Just d) -> generateFwdDecl (d_name d) d
           Nothing -> error "BUG: forward decl needed for unknown type"

       -- Now do a topological sort on all the decls, ignoring the forward references
       sortedDecls = case topologicalSort fst (ignoreNameOnly . referencedLocalTypes' . snd) (Map.toList (m_decls m)) of
         Nothing -> error "BUG: Unable to sort decls into compilable order (possible infinite type??)"
         Just decls -> decls

       genDecl (n,d) = case d_customType d of
         (Just ct) -> generateCustomType n d ct
         Nothing -> generateDecl (d_name d) d

   includeStd ifile "adl/adl.h"
   includeModule cppfile mname

   mapM_ genFwdDecl fwdRefs
   mapM_ genDecl sortedDecls

-- | Generate the c++ code for an ADL module into files.
-- The call @writeModuleFile uo mNamespace mFile module@ will use
-- @mNamespace@ to map from ADL Modules to C++ namespaces, and @mFile@
-- to map from ADL Modules to actual file system paths. If @uo@ is
-- true, then the output files will not be written if there are
-- existing identical files.
writeModuleFile :: (ModuleName -> CppNamespace) ->
                   (ModuleName -> FilePath) ->
                   (ModuleName -> FilePath) ->
                   (FilePath -> LBS.ByteString -> IO ()) ->
                   CModule ->
                   EIO a ()
writeModuleFile mNamespace mIncFile mFile fileWriter m = do
  let fs0 = FState Set.empty  [] (mNamespace (m_name m))
      fs1 = FState Set.empty  [] (CppNamespace ["ADL"])
      s0 = MState (m_name m) mNamespace mIncFile fs0 fs1 fs0 fs1
      s1 = execState (generateModule m) s0
      guard_name = (T.append (T.intercalate "_" (map T.toUpper (unModuleName (m_name m)))) "_H" )

      ifileElements = [ms_incFileUserModule s1,ms_incFileSerialisation s1]
      ifileLines = [ template "#ifndef $1" [guard_name]
                   , template "#define $1" [guard_name]
                   ] ++ fileLines ifileElements ++
                   [ template "#endif // $1" [guard_name]
                   ]

      cppfileElements = [ms_cppFileUserModule s1,ms_cppFileSerialisation s1]
      cppfileLines = fileLines cppfileElements

  liftIO $ fileWriter (mIncFile (m_name m) ++ ".h") (LBS.fromStrict (T.encodeUtf8 (T.intercalate "\n" ifileLines )))
  liftIO $ fileWriter (mFile (m_name m) ++ ".cpp") (LBS.fromStrict (T.encodeUtf8 (T.intercalate "\n" cppfileLines)))

data CppFlags = CppFlags {
  -- all include files will be generated and referenced
  -- with this path prefix
  cf_incFilePrefix :: FilePath
  }


cppCustomType :: ScopedName
cppCustomType = ScopedName (ModuleName ["adlc","config","cpp"]) "CppCustomType"


getCustomType :: ScopedName -> RDecl -> Maybe CustomType
getCustomType scopedName decl = case Map.lookup cppCustomType (d_annotations decl) of
  Nothing -> Nothing
  Just (_,json) -> Just (convertCustomType json)
  where
    convertCustomType :: JSON.Value -> CustomType
    convertCustomType jv = case adlFromJson jv of
      (ParseFailure e ctx) -> error (T.unpack (  "BUG: failed to parse java custom type: " <> e
                                    <> ", at " <> textFromParseContext ctx))
      (ParseSuccess cct) -> CustomType
        { ct_name = CC.cppCustomType_cppname cct
        , ct_includes = Set.fromList (map mkInc (CC.cppCustomType_cppincludes cct))
        , ct_generateOrigADLType = convertOrigType (CC.cppCustomType_generateOrigADLType cct)
        , ct_declarationCode = CC.cppCustomType_declarationCode cct
        , ct_serialisationCode = CC.cppCustomType_serialisationCode cct
        }
      where
        parseScopedName :: T.Text -> ScopedName
        parseScopedName t = case P.parse P.scopedName "" t of
          (Right sn) -> sn
          _ -> error ("failed to parse scoped name '" <> T.unpack t <> "' in java custom type for " <> T.unpack (formatText scopedName))

        mkInc i = IncFilePath (T.unpack (CC.include_name i)) (not (CC.include_system i))

        convertOrigType ot | ot == "" = Nothing
                           | otherwise = Just ot

namespaceGenerator :: ModuleName -> CppNamespace
namespaceGenerator mn = CppNamespace ("ADL":unModuleName mn)

incFileGenerator :: FilePath -> ModuleName -> FilePath
incFileGenerator prefix mn = prefix </> T.unpack (T.intercalate "." (unModuleName mn))

fileGenerator :: ModuleName -> FilePath
fileGenerator mn = T.unpack (T.intercalate "." (unModuleName mn))

generate :: AdlFlags -> CppFlags -> FileWriter -> [FilePath] -> EIOT ()
generate af cf fileWriter modulePaths = catchAllExceptions  $ forM_ modulePaths $ \modulePath -> do
  m0 <- loadAndCheckModule af modulePath
  let m = associateCustomTypes getCustomType (m_name m0) m0
  checkCustomSerializations m
  writeModuleFile namespaceGenerator
                  (incFileGenerator (cf_incFilePrefix cf))
                  fileGenerator
                  fileWriter
                  m

----------------------------------------------------------------------

literalLValue :: (Literal CTypeExpr) -> Gen T.Text
literalLValue (Literal te@(TypeExpr (RT_Primitive pt) []) LDefault) = do
  case cPrimitiveDefault pt of
   (Just t) -> return t
   Nothing -> do
     t <- cTypeExpr False te
     return (template "$1()" [t])
literalLValue (Literal te LDefault) = do
  t <- cTypeExpr False te
  return (template "$1()" [t])
literalLValue (Literal te (LCtor ls)) = do
  t <- cTypeExpr False te
  lits <- mapM literalLValue ls
  return (template "$1($2)" [t, T.intercalate "," lits])
literalLValue (Literal te (LUnion ctor l)) = do
  t <- cTypeExpr False te
  lit <-  literalLValue l
  if isVoidLiteral l
    then return (template "$1::$2()" [t,cUnionConstructorName ctor])
    else return (template "$1::$2($3)" [t,cUnionConstructorName ctor,lit])
literalLValue (Literal (TypeExpr _ [te]) (LVector ls)) = do
  t <- cTypeExpr False te
  lits <- mapM literalLValue ls
  return (template "mkvec<$1>($2)" [t, T.intercalate "," lits])
literalLValue (Literal _ (LVector ls)) = error "BUG: LVector must have a single type param"
literalLValue (Literal (TypeExpr _ [te]) (LStringMap map)) = do
  t <- cTypeExpr False te
  adds <- forM (Map.toList map) $ \(k,v) -> do
    litv <- literalLValue v
    return (template ".add(\"$1\",$2)" [k,litv])
  return (template "MapBuilder<std::string,$1>()$2.result()" [t, T.intercalate "" adds])
literalLValue (Literal (TypeExpr (RT_Primitive pt) _) (LPrimitive  jv)) = return (cPrimitiveLiteral pt jv)
literalLValue _ = error "BUG: literalLValue: unexpected literal value"

literalPValue :: (Literal CTypeExpr) -> Gen T.Text
literalPValue (Literal te@(TypeExpr (RT_Primitive pt) []) LDefault) = do
  t <- cTypeExpr False te
  case cPrimitiveDefault pt of
   (Just v) -> return (template "$1($2)" [t,v])
   Nothing -> return (template "$1()" [t])
literalPValue l@(Literal _ LDefault) = literalLValue l
literalPValue l@(Literal _ (LCtor _)) = literalLValue l
literalPValue l@(Literal te (LUnion _ _)) = do
  t <- cTypeExpr False te
  lit <- literalLValue l
  return $ template "$1($2)" [t,lit]
literalPValue l@(Literal te (LVector _)) = do
  t <- cTypeExpr False te
  lit <- literalLValue l
  return (template "std::vector<$1>( $2 )" [t,lit])
literalPValue l@(Literal _ (LStringMap _)) = literalLValue l
literalPValue (Literal (TypeExpr (RT_Primitive pt) _) (LPrimitive jv)) = do
  t <- cPrimitiveType pt []
  return (template "$1($2)" [t, cPrimitiveLiteral pt jv])
literalPValue _ = error "BUG: literalPValue: unexpected literal value"

literalNeedsInit :: Literal CTypeExpr -> Bool
literalNeedsInit (Literal  (TypeExpr (RT_Primitive pt) _) LDefault) = isJust (cPrimitiveDefault pt)
literalNeedsInit (Literal _ LDefault)  = False
literalNeedsInit _  = True

----------------------------------------------------------------------

intType :: T.Text -> Gen T.Text
intType s = includeStd ifile "stdint.h" >> return s

cPrimitiveType :: PrimitiveType -> [T.Text] -> Gen T.Text
cPrimitiveType P_Void _ = return "Void"
cPrimitiveType P_Bool _ = return "bool"
cPrimitiveType P_Int8 _ = intType "int8_t"
cPrimitiveType P_Int16 _ = intType "int16_t"
cPrimitiveType P_Int32 _ = intType "int32_t"
cPrimitiveType P_Int64 _ = intType "int64_t"
cPrimitiveType P_Word8 _ = intType "uint8_t"
cPrimitiveType P_Word16 _ = intType "uint16_t"
cPrimitiveType P_Word32 _ = intType "uint32_t"
cPrimitiveType P_Word64 _ = intType "uint64_t"
cPrimitiveType P_Float _ = return "float"
cPrimitiveType P_Double _ = return "double"
cPrimitiveType P_Json _ = return "JsonValue"
cPrimitiveType P_ByteVector _ = return "ByteVector"
cPrimitiveType P_Vector targs = do
  includeStd ifile "vector"
  return (template "std::vector<$1> " targs)
cPrimitiveType P_StringMap targs = do
  includeStd ifile "string"
  includeStd ifile "map"
  return (template "std::map<std::string,$1>" targs)
cPrimitiveType P_Nullable targs = do
  return (template "nullable<$1>" targs)
cPrimitiveType P_String targs = do
  includeStd ifile "string"
  return "std::string"

cPrimitiveDefault :: PrimitiveType -> Maybe T.Text
cPrimitiveDefault P_Void = Nothing
cPrimitiveDefault P_Bool = Just "false"
cPrimitiveDefault P_Int8 = Just "0"
cPrimitiveDefault P_Int16 = Just "0"
cPrimitiveDefault P_Int32 = Just "0"
cPrimitiveDefault P_Int64 = Just "0"
cPrimitiveDefault P_Word8 = Just "0"
cPrimitiveDefault P_Word16 = Just "0"
cPrimitiveDefault P_Word32 = Just "0"
cPrimitiveDefault P_Word64 = Just "0"
cPrimitiveDefault P_Float = Just "0.0"
cPrimitiveDefault P_Double = Just "0.0"
cPrimitiveDefault P_Json = Just "JsonValue::null"
cPrimitiveDefault P_ByteVector = Nothing
cPrimitiveDefault P_Vector = Nothing
cPrimitiveDefault P_StringMap = Nothing
cPrimitiveDefault P_Nullable = Nothing
cPrimitiveDefault P_String = Nothing

cPrimitiveLiteral :: PrimitiveType -> JSON.Value -> T.Text
cPrimitiveLiteral P_Void _ = "Void()"
cPrimitiveLiteral P_Bool (JSON.Bool True) = "true"
cPrimitiveLiteral P_Bool (JSON.Bool False) = "false"
cPrimitiveLiteral P_Int8 (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Int16 (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Int32 (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Int64 (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Word8 (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Word16 (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Word32 (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Word64 (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Float (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Double (JSON.Number n) = litNumber n
cPrimitiveLiteral P_Json (jv) = template "JsonValue.parseString($1)" [T.pack (show (JSON.encode jv))]
cPrimitiveLiteral P_ByteVector (JSON.String s) = template "ByteVector::fromLiteral($1)" [T.pack (show (decode s))]
  where
    decode s = case B64.decode (T.encodeUtf8 s) of
      (Left _) -> "???"
      (Right s) -> s
cPrimitiveLiteral P_Vector _ = "????" -- never called
cPrimitiveLiteral P_StringMap _ = "????" -- never called
cPrimitiveLiteral P_Nullable _ = "????" -- never called
cPrimitiveLiteral P_String (JSON.String s) = T.pack (show s)
cPrimitiveLiteral _ _ = error "BUG: invalid json literal for primitive"
