{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module ADL.Compiler.Backends.Cpp(
  generate,
  CppFlags(..)
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L
import Data.Ord (comparing)

import System.FilePath(joinPath,takeDirectory,(</>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import qualified Data.Scientific as S

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS

import qualified Text.Parsec as P
import qualified ADL.Compiler.ParserP as P

import ADL.Utils.Format
import ADL.Compiler.AST
import ADL.Compiler.Backends.Literals
import ADL.Compiler.EIO
import ADL.Compiler.Processing
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
   ms_customTypes :: Map.Map ScopedName CustomType,
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

type CustomTypeMap = Map.Map ScopedName CustomType

type Gen = State MState

instance MGen (State MState) where
  getPrimitiveType = cPrimitiveType
  getPrimitiveDefault pt = return (cPrimitiveDefault pt)
  getPrimitiveLiteral pt jv = return (cPrimitiveLiteral pt jv)
  getTypeExpr = cTypeExpr
  getTypeExprB = cTypeExprB
  getUnionConstructorName d f = return (cUnionConstructorName d f)

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

type TypeBindingMap = Map.Map Ident (TypeExpr ResolvedType)

-- Returns the c++ typer expression corresponding to the
-- given ADL type expression
cTypeExpr :: Bool -> TypeExpr ResolvedType -> Gen T.Text
cTypeExpr scopeLocalNames te = cTypeExprB scopeLocalNames Map.empty te

cTypeExprB :: Bool -> TypeBindingMap -> TypeExpr ResolvedType -> Gen T.Text
cTypeExprB scopeLocalNames m (TypeExpr rt []) = cTypeExprB1 scopeLocalNames m rt
cTypeExprB scopeLocalNames m (TypeExpr c args) = do
  ct <- cTypeExprB1 scopeLocalNames m c
  argst <- mapM (cTypeExprB scopeLocalNames m) args
  return (T.concat $ [ct, "<"] ++ L.intersperse "," argst ++ ["> "])

cTypeExprB1 :: Bool -> TypeBindingMap -> ResolvedType -> Gen T.Text
cTypeExprB1 scopeLocalNames _ (RT_Named (sn,_)) = do
  ms <- get
  let isLocalName = case sn_moduleName sn of
        ModuleName [] -> True
        _ -> False
      fullyScopedName = if isLocalName then sn{sn_moduleName=ms_name ms} else sn

  case Map.lookup fullyScopedName (ms_customTypes ms) of
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
        
cTypeExprB1 scopeLocalNames m (RT_Param i) = case Map.lookup i m of
    (Just te) -> cTypeExprB scopeLocalNames m te
    Nothing -> return (cTypeParamName i)
cTypeExprB1 scopeLocalNames _ (RT_Primitive pt) = cPrimitiveType pt

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
cFieldName :: Ident -> Ident -> Ident
cFieldName _ = unreserveWord

-- Returns the c++ name for the accessor function for a union field
cUnionAccessorName :: Decl t -> Field t  -> Ident
cUnionAccessorName _ f = unreserveWord (f_name f)

-- Returns the c++ name for the constructor function for a union field
cUnionConstructorName :: Decl t -> Field t -> Ident
cUnionConstructorName d f = T.append "mk_" (cUnionAccessorName d f)

-- Returns the c++ name for the setter function for a union field
cUnionSetterName :: Decl t -> Field t -> Ident
cUnionSetterName d f = T.append "set_" (cUnionAccessorName d f)

-- Returns the c++ name for the enum value corresponding to the ADL discriminator name
cUnionDiscName :: Decl t -> Field t -> Ident
cUnionDiscName t f = T.toUpper (cUnionAccessorName t f)

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

generateFwdDecl1 :: Ident -> [Ident] -> Gen ()
generateFwdDecl1 dn tparams = do
  write ifile $ do        
    wl ""
    genTemplate tparams
    wt "struct $1;" [cTypeName dn]
  
generateFwdDecl :: Ident -> Decl ResolvedType -> Gen ()
generateFwdDecl dn (Decl{d_type=(Decl_Struct s)}) = generateFwdDecl1 dn (s_typeParams s)
generateFwdDecl dn (Decl{d_type=(Decl_Union u)}) = generateFwdDecl1 dn (u_typeParams u)
generateFwdDecl dn (Decl{d_type=(Decl_Newtype n)}) = generateFwdDecl1 dn (n_typeParams n)
generateFwdDecl _ (Decl{d_type=(Decl_Typedef _)}) = error "BUG: Unexpected fwd declaration of typedef"

generateDecl :: Ident -> Decl ResolvedType -> Gen ()
generateDecl dn d@(Decl{d_type=(Decl_Struct s)}) = do
  ms <- get
  fts <- forM (s_fields s) $ \f -> do
    t <- cTypeExpr False (f_type f)
    scopedt <- cTypeExpr True (f_type f)
    litv <- case f_default f of
        (Just v) -> mkLiteral (f_type f) v
        Nothing -> mkDefaultLiteral (f_type f)
    return (cFieldName dn (f_name f), f, t, scopedt, litv)

  let ns = ms_moduleMapper ms (ms_name ms)
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
       wt "$1(" [ctname]
       indent $ do
         forM_ (commaSep fts) $ \((fname,f,t,_,_),sep) -> do
            wt "const $1 & $2$3" [t, fname,sep]
         wl ");"
       wl ""
       forM_ fts $ \(fname,_,t,_,_) -> do
           wt "$1 $2;" [t, fname]

    declareOperators ifile (s_typeParams s) ctnameP

  write icfile $ do
    -- Constructors
    wl ""
    genTemplate (s_typeParams s)
    wt "$1::$2()" [ctnameP, ctname]
    indent $ do
      let ifts = [ v | v@(_,_,_,_,litv) <- fts, not (literalIsDefault litv) ]
      forM_ (addMarker ":" "," "," ifts) $ \(mark,(fname,f,t,_,litv)) -> do
          wt "$1 $2($3)" [mark,fname,literalLValue litv]
    cblock $ return ()
    wl ""
    genTemplate (s_typeParams s)
    wt "$1::$2(" [ctnameP,ctname]
    indent $ do
      forM_ (commaSep fts) $ \((fname, f,t,_,_),sep) -> do
        wt "const $1 & $2_$3" [t,fname,sep]
      wl ")"
      forM_ (addMarker ":" "," "," fts) $ \(mark,(fname, f,t,_,_)) -> do
        wt "$1 $2($2_)" [mark,fname]
    cblock $ return ()

    -- Non-inline functions
    wl ""
    genTemplate (s_typeParams s)
    wl "bool"
    wt "operator<( const $1 &a, const $1 &b )" [ctnameP]
    cblock $ do
      forM_ fts $ \(fname, f,t,_,_) -> do
        wt "if( a.$1 < b.$1 ) return true;" [fname]
        wt "if( b.$1 < a.$1 ) return false;" [fname]
      wl "return false;"
    wl ""
    genTemplate (s_typeParams s)
    wl "bool"
    wt "operator==( const $1 &a, const $1 &b )" [ctnameP]
    cblock $ do 
      wl "return"
      forM_ (sepWithTerm "&&" ";" fts) $ \((fname, f,t,_,_),sep) -> do
        indent $ wt "a.$1 == b.$1 $2" [fname,sep]

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
              forM_ (addMarker ":" "," "," fts) $ \(mark,(fname,_,_,scopedt,_)) -> do
                wt "$1 $2_s( Serialisable<$3>::serialiser(sf) )" [mark,fname,scopedt]
              wl "{}"
            wl ""
            wl ""
            forM_ fts $ \(fname,_,_,scopedt,_) -> do
              wt "typename Serialiser<$1>::Ptr $2_s;" [scopedt,fname]
            wl ""
            wl "void toJson( JsonWriter &json, const _T & v ) const"
            cblock $ do
              wl "json.startObject();"
              forM_ fts $ \(fname, f,_,scopedt,_) -> do
                wt "writeField<$1>( json, $3_s, \"$2\", v.$3 );" [scopedt,f_name f,fname]
              wl "json.endObject();"
              return ()
            wl ""
            wl "void fromJson( _T &v, JsonReader &json ) const"
            cblock $ do
              wl "match( json, JsonReader::START_OBJECT );"
              wl "while( !match0( json, JsonReader::END_OBJECT ) )"
              cblock $ do
                forM_ fts $ \(fname, f,_,_,_) -> do
                  wt "readField( $1_s, v.$1, \"$2\", json ) ||" [fname,f_name f]
                wl "ignoreField( json );"
        wl ""
        wl "return typename Serialiser<_T>::Ptr( new S_(sf) );"

generateDecl dn d@(Decl{d_type=(Decl_Union u)}) = do
  ms <- get
  fts <- forM (u_fields u) $ \f -> do
    t <- cTypeExpr False (f_type f)
    scopedt <- cTypeExpr True (f_type f)
    litv <- case f_default f of
        (Just v) -> mkLiteral (f_type f) v
        Nothing -> mkDefaultLiteral (f_type f)
    return (f, t, scopedt, litv)

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
      forM_ fts $ \(f,t,_,_) -> do
        let ctorName = cUnionConstructorName d f
        if isVoidType (f_type f)
          then wt "static $1 $2();" [ctnameP, ctorName ] 
          else wt "static $1 $2( const $3 & v );" [ ctnameP, ctorName, t ]

      wl ""
      wt "$1( const $2 & );" [ctname,ctnameP]
      wt "~$1();" [ctname]
      wt "$1 & operator=( const $1 & );" [ctnameP]
      wl ""
      wl "enum DiscType"
      dblock $ 
        forM_ (commaSep fts) $ \((f,t,_,_),sep) -> do
          wt "$1$2" [cUnionDiscName d f, sep]
      wl ""
      wl "DiscType d() const;"
      forM_ fts $ \(f,t,_,_) -> do
        when (not $ isVoidType (f_type f)) $
          wt "$1 & $2() const;" [t,cUnionAccessorName d f]
      wl ""
      forM_ fts $ \(f,t,_,_) -> do
        if isVoidType (f_type f)
          then wt "void $1();" [cUnionSetterName d f]
          else wt "const $1 & $2(const $1 & );" [t,cUnionSetterName d f]
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

    forM_ fts $ \(f,t,_,_) -> do
      when (not $ isVoidType (f_type f)) $ do
        wl ""
        genTemplate (u_typeParams u)
        wt "inline $1 & $2::$3() const" [t,ctnameP,cUnionAccessorName d f]
        cblock $ do
          wt "if( d_ == $1 )" [cUnionDiscName d f]
          cblock $ do
            wt "return *($1 *)p_;" [t]
          wl "throw invalid_union_access();"

  write icfile $ do
    wl ""
    genTemplate (u_typeParams u)
    wt "$1::$2()" [ctnameP,ctname]
    -- FIXME :: Confirm that typechecker disallows empty unions, so the
    -- head below is ok.
    let (f,t,_,litv) = head fts
        lv = if isVoidType (f_type f)
             then "0"
             else template "new $1" [literalPValue litv]
    indent $ wt ": d_($1), p_($2)" [cUnionDiscName d f,lv]
    wl "{"
    wl "}"
    forM_ fts $ \(f,t,_,_) -> do
      let ctorName = cUnionConstructorName d f
      wl ""
      genTemplate (u_typeParams u)
      if isVoidType (f_type f)
        then do
          wt "$1 $1::$2()" [ctnameP, ctorName ]
          cblock $
            wt "return $1( $2, 0 );" [ctnameP, cUnionDiscName d f]
        else do
          wt "$1 $1::$2( const $3 & v )" [ ctnameP, ctorName, t ]
          cblock $
            wt "return $1( $2, new $3(v) );" [ctnameP, cUnionDiscName d f,t]
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

    forM_ fts $ \(f,t,_,_) -> do
      wl ""
      genTemplate (u_typeParams u)
      if isVoidType (f_type f)
        then do
          wt "void $1::$2()" [ctnameP,cUnionSetterName d f]
          cblock $ do
            wt "if( d_ != $1 )" [cUnionDiscName d f]
            cblock $ do
              wl "free(d_,p_);"
              wt "d_ = $1;" [cUnionDiscName d f]
              wt "p_ = 0;" [cUnionDiscName d f]

        else do
          wt "const $1 & $2::$3(const $1 &v)" [t,ctnameP,cUnionSetterName d f]
          cblock $ do
            wt "if( d_ == $1 )" [cUnionDiscName d f]
            cblock $ do
              wt "*($1 *)p_ = v;" [t]
            wl "else"
            cblock $ do
              wl "free(d_,p_);"
              wt "d_ = $1;" [cUnionDiscName d f]
              wt "p_ = new $1(v);" [t]
            wt "return *($1 *)p_;" [t]

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
        forM_ fts $ \(f,t,_,_) -> do
          if isVoidType (f_type f)
            then wt "case $1: return;"
                 [cUnionDiscName d f]
            else wt "case $1: delete ($2 *)p; return;"
                 [cUnionDiscName d f,t]
    wl ""
    genTemplate (u_typeParams u)
    wt "void * $1::copy( DiscType d, void *p )" [ctnameP]
    cblock $ do
      wl "switch( d )"
      cblock $ 
        forM_ fts $ \(f,t,_,_) -> do
          if isVoidType (f_type f)
            then wt "case $1: return 0;"
                 [cUnionDiscName d f]
            else wt "case $1: return new $2(*($2 *)p);"
                 [cUnionDiscName d f,t]
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
        forM_ fts $ \(f,t,_,_) -> do
          if isVoidType (f_type f)
            then wt "case $1::$2: return false;"
                 [ctnameP,cUnionDiscName d f]
            else wt "case $1::$2: return a.$3() < b.$3();"
                 [ctnameP,cUnionDiscName d f,cUnionAccessorName d f]
      wl "return false;"

    wl ""
    genTemplate (u_typeParams u)
    wl "bool"
    wt "operator==( const $1 &a, const $1 &b )" [ctnameP]
    cblock $ do 
      wl "if( a.d() != b.d() ) return false;"
      wl "switch( a.d() )"
      cblock $ 
        forM_ fts $ \(f,t,_,_) -> do
          if isVoidType (f_type f)
            then wt "case $1::$2: return true;"
                 [ctnameP,cUnionDiscName d f]
            else wt "case $1::$2: return a.$3() == b.$3();"
                 [ctnameP,cUnionDiscName d f,cUnionAccessorName d f]
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
        wl "struct S_ : public Serialiser<_T>"
        dblock $ do
            wl "S_( const SerialiserFlags & sf )"
            indent $ do
              wl ": sf_(sf)"
              wl "{}"
            wl ""
            wl "SerialiserFlags sf_;";
            forM_ fts $ \(f,_,scopedt,_) -> do
              wt "mutable typename Serialiser<$1>::Ptr $2_;" [scopedt,f_name f]
            wl ""
            forM_ fts $ \(f,_,scopedt,_) -> do
              wt "typename Serialiser<$1>::Ptr $2_s() const" [scopedt,f_name f]
              cblock $ do
                wt "if( !$1_ )" [f_name f]
                indent $ wt "$1_ = Serialisable<$2>::serialiser(sf_);" [f_name f,scopedt]
                wt "return $1_;" [f_name f]
              wl ""
            wl "void toJson( JsonWriter &json, const _T & v ) const"
            cblock $ do
              wl "json.startObject();"
              wl "switch( v.d() )"
              cblock $ do
                forM_ fts $ \(f,_,_,_) -> do
                   let v | isVoidType (f_type f) = "Void()"
                         | otherwise = template "v.$1()" [cUnionAccessorName d f]

                   wt "case $1::$2: writeField( json, $3_s(), \"$3\", $4 ); break;"
                     [scopedctnameP,cUnionDiscName d f, f_name f,v]
              wl "json.endObject();"
              return ()
            wl ""
            wl "void fromJson( _T &v, JsonReader &json ) const"
            cblock $ do
              wl "match( json, JsonReader::START_OBJECT );"
              wl "while( !match0( json, JsonReader::END_OBJECT ) )"
              cblock $ do
                forM_ (addMarker "if" "else if" "else if" fts) $ \(ifcmd,(f,_,_,_)) -> do
                  wt "$1( matchField0( \"$2\", json ) )" [ifcmd,f_name f]
                  if isVoidType (f_type f)
                    then cblock $ do
                      wt "$1_s()->fromJson( json );" [f_name f]
                      wt "v.$1();" [cUnionSetterName d f]
                    else indent $ do
                      wt "v.$1($2_s()->fromJson( json ));" [cUnionSetterName d f,f_name f]
                wl "else"
                indent $ wl "throw json_parse_failure();"
        wl ""
        wl "return typename Serialiser<_T>::Ptr( new S_(sf) );"

generateDecl dn d@(Decl{d_type=(Decl_Newtype nt)}) = do
  ms <- get
  t <- cTypeExpr False (n_typeExpr nt)
  scopedt <- cTypeExpr True (n_typeExpr nt)
  litv <- case n_default nt of
    (Just v) -> mkLiteral (n_typeExpr nt) v
    Nothing -> mkDefaultLiteral (n_typeExpr nt)

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
       if literalIsDefault litv
         then wt "$1() {}" [ctname]
         else wt "$1() : value($2) {}" [ctname,literalLValue litv]

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
        wt "return typename Serialiser<$1>::Ptr(new S(Serialisable<$2>::serialiser(sf)));" [ctnameP1,scopedt]

generateDecl dn d@(Decl{d_type=(Decl_Typedef t)}) = do
  te <- cTypeExpr False (t_typeExpr t)
  write ifile $ do
    wl ""
    genTemplate (t_typeParams t)
    wt "using $1 = $2;" [cTypeName dn, te]

data TypeRefType a = NameOnly a | FullDecl a
   deriving (Eq,Ord,Show)                

nameOnly :: TypeRefType a -> TypeRefType a
nameOnly (FullDecl a) = (NameOnly a)
nameOnly t = t

ignoreNameOnly :: Ord a => Set.Set (TypeRefType a) -> Set.Set a
ignoreNameOnly ts = Set.fromList [ a | (FullDecl a) <- Set.toList ts]

localTypes :: TypeExpr ResolvedType -> Set.Set (TypeRefType Ident)
localTypes (TypeExpr (RT_Primitive P_Vector) [te]) = Set.map nameOnly (localTypes te)
localTypes (TypeExpr c args) = Set.unions (localTypes1 c:[localTypes a | a <- args])

localTypes1 :: ResolvedType -> Set.Set (TypeRefType Ident)
localTypes1 (RT_Named (sn,_)) = case sn_moduleName sn of
    ModuleName [] -> Set.singleton (FullDecl (sn_name sn))
    -- FIXME: need to either check if fully scoped name matches current module here,
    -- or, alternatively, map fully scoped local references to unscoped ones as a compiler
    -- phase.
    _ -> Set.empty 
localTypes1 (RT_Param _) = Set.empty
localTypes1 (RT_Primitive _) = Set.empty

referencedLocalTypes :: Decl ResolvedType -> Set.Set (TypeRefType Ident)
referencedLocalTypes d = Set.difference (rtypes d) selfrefs
  where
    selfrefs = Set.fromList [NameOnly (d_name d), FullDecl (d_name d)]
    rtypes (Decl{d_type=(Decl_Struct s)}) = Set.unions [ localTypes (f_type f) | f <- s_fields s]
    rtypes (Decl{d_type=(Decl_Union u)}) = Set.map nameOnly (Set.unions [ localTypes (f_type f) | f <- u_fields u])
    rtypes (Decl{d_type=(Decl_Typedef t)}) = localTypes (t_typeExpr t)
    rtypes (Decl{d_type=(Decl_Newtype n)}) = localTypes (n_typeExpr n)
    
generateCustomType :: Ident -> Decl ResolvedType -> CustomType -> Gen ()
generateCustomType n d ct = do
  ms <- get
  mapM_ (include0 ifile) (Set.toList (ct_includes ct))

  write ifile $ wl ""
  case ct_generateOrigADLType ct of
    Nothing -> do
      write ifile $ wt "// $1 excluded due to custom definition" [n]
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
expandUVTypedefs :: Module ResolvedType -> Module ResolvedType
expandUVTypedefs m = m{m_decls=decls}
  where
    decls = Map.map (\d->d{d_type=expand1 (d_type d)}) (m_decls m)
    expand1 :: DeclType ResolvedType -> DeclType ResolvedType
    expand1 (Decl_Struct s) = Decl_Struct s{s_fields=map expandSField (s_fields s)}
    expand1 (Decl_Union u) = Decl_Union u{u_fields=map expandUField (u_fields u)}
    expand1 (Decl_Typedef t) = Decl_Typedef t{t_typeExpr=expandVectors (t_typeExpr t)}
    expand1 (Decl_Newtype n) = Decl_Newtype n{n_typeExpr=expandVectors (n_typeExpr n)}

    expandSField :: Field ResolvedType -> Field ResolvedType
    expandSField f = f{f_type=expandVectors (f_type f)}

    expandUField :: Field ResolvedType -> Field ResolvedType
    expandUField f = f{f_type=expandAll (f_type f)}

    -- expand all typedefs present in the given expression
    expandAll :: TypeExpr ResolvedType -> TypeExpr ResolvedType
    expandAll = expandTypedefs

    -- expand only typedefs within Vector<> applications
    expandVectors :: TypeExpr ResolvedType -> TypeExpr ResolvedType
    expandVectors te@(TypeExpr (RT_Primitive P_Vector) _) = expandAll te
    expandVectors (TypeExpr rt tes) = TypeExpr rt (map expandVectors tes)

generateModule :: Module ResolvedType -> Gen ()
generateModule m0 = do
   ms <- get
   let mname = ms_name ms

       m = expandUVTypedefs m0

       getCustomType n = Map.lookup (ScopedName mname n) (ms_customTypes ms)

       makeCustomTypeFullRef :: TypeRefType Ident -> TypeRefType Ident
       makeCustomTypeFullRef r@(NameOnly n) = case getCustomType n of
         Nothing -> r
         (Just _) -> (FullDecl n)
       makeCustomTypeFullRef r = r

       referencedLocalTypes' = Set.map makeCustomTypeFullRef . referencedLocalTypes

       -- generate the types for which we want forward references
       allRefs = (Set.unions . map referencedLocalTypes' . Map.elems . m_decls) m
       fwdRefs = [ n | (NameOnly n) <- (Set.toList . Set.unions . map referencedLocalTypes' . Map.elems . m_decls)  m]

       genFwdDecl n = case Map.lookup n (m_decls m) of
           (Just d) -> generateFwdDecl (d_name d) d
           Nothing -> error "BUG: forward decl needed for unknown type"

       -- Now do a topological sort on all the decls, ignoring the forward references
       sortedDecls = case topologicalSort fst (ignoreNameOnly . referencedLocalTypes' . snd) (Map.toList (m_decls m)) of
         Nothing -> error "BUG: Unable to sort decls into compilable order (possible infinite type??)"
         Just decls -> decls

       genDecl (n,d) = case getCustomType n of
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
                   CustomTypeMap -> 
                   (FilePath -> LBS.ByteString -> IO ()) ->
                   Module ResolvedType ->
                   EIO a ()
writeModuleFile mNamespace mIncFile mFile customTypes fileWriter m = do
  let fs0 = FState Set.empty  [] (mNamespace (m_name m))
      fs1 = FState Set.empty  [] (CppNamespace ["ADL"])
      s0 = MState (m_name m) mNamespace mIncFile customTypes fs0 fs1 fs0 fs1
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
  -- directories where we look for ADL files
  cf_searchPath :: [FilePath],

  -- all include files will be generated and referenced
  -- with this path prefix
  cf_incFilePrefix :: FilePath,

  -- Files containing custom type definitions
  cf_customTypeFiles :: [FilePath],

  cf_fileWriter :: FilePath -> LBS.ByteString -> IO ()
  }

getCustomTypes :: [FilePath] -> EIOT CustomTypeMap
getCustomTypes fps = fmap Map.unions (mapM get0 fps)
  where
    get0 :: FilePath -> EIOT CustomTypeMap
    get0 fp = do
      mv <- liftIO $ aFromJSONFile (jsonSerialiser jsflags) fp
      case mv of
        Nothing -> eioError (template "Unable to read haskell custom types from  $1" [T.pack fp])
        Just v -> convert (CC.config_customTypes v)
    jsflags = JSONFlags True

    convert :: [CC.CustomType] -> EIOT CustomTypeMap
    convert cs = fmap Map.fromList (mapM convert1 cs)

    convert1 :: CC.CustomType -> EIOT (ScopedName,CustomType)
    convert1 c = do
        sn <- case P.parse P.scopedName "" adlname of
          (Right sn) -> return sn
          _ -> eioError (template "Unable to parse adl name $1" [adlname])
        let ct = CustomType (CC.customType_cppname c) includes (CC.customType_generateOrigADLType c) (CC.customType_declarationCode c) (CC.customType_serialisationCode c)
        return (sn,ct)
      where
        adlname = CC.customType_adlname c
        includes = Set.fromList (map mkInc (CC.customType_cppincludes c))
        mkInc i = IncFilePath (T.unpack (CC.include_name i)) (not (CC.include_system i))

namespaceGenerator :: ModuleName -> CppNamespace
namespaceGenerator mn = CppNamespace ("ADL":unModuleName mn)

incFileGenerator :: FilePath -> ModuleName -> FilePath
incFileGenerator prefix mn = prefix </> T.unpack (T.intercalate "." (unModuleName mn))

fileGenerator :: ModuleName -> FilePath
fileGenerator mn = T.unpack (T.intercalate "." (unModuleName mn))

generate :: CppFlags -> [FilePath] -> EIOT ()
generate cf modulePaths = catchAllExceptions  $ forM_ modulePaths $ \modulePath -> do
  rm <- loadAndCheckModule (moduleFinder (cf_searchPath cf)) modulePath
  customTypes <- getCustomTypes (cf_customTypeFiles cf)
  writeModuleFile namespaceGenerator
                  (incFileGenerator (cf_incFilePrefix cf))
                  fileGenerator
                  customTypes
                  (cf_fileWriter cf)
                  rm

----------------------------------------------------------------------

literalLValue :: Literal -> T.Text
literalLValue (LDefault t) = template "$1()" [t]
literalLValue (LCtor t ls) = template "$1($2)" [t, T.intercalate "," (map literalLValue ls)]
literalLValue (LUnion t ctor l) = template "$1::$2($3)" [t, ctor, literalLValue l ]
literalLValue (LVector t ls) = template "mkvec<$1>($2)" [t, T.intercalate "," (map literalLValue ls)]
literalLValue (LPrimitive _ t) = t

literalPValue :: Literal -> T.Text
literalPValue l@(LDefault _) = literalLValue l
literalPValue l@(LCtor _ _) = literalLValue l
literalPValue l@(LUnion t _ _) = template "$1($2)" [t, literalLValue l]
literalPValue l@(LVector t _) = template "std::vector<$1>( $2 )" [t, literalLValue l]
literalPValue (LPrimitive t v) = template "$1($2)" [t, v]

----------------------------------------------------------------------

intType :: T.Text -> Gen T.Text
intType s = includeStd ifile "stdint.h" >> return s

cPrimitiveType :: PrimitiveType -> Gen T.Text
cPrimitiveType P_Void = return "Void"
cPrimitiveType P_Bool = return "bool"
cPrimitiveType P_Int8 = intType "int8_t"
cPrimitiveType P_Int16 = intType "int16_t"
cPrimitiveType P_Int32 = intType "int32_t"
cPrimitiveType P_Int64 = intType "int64_t"
cPrimitiveType P_Word8 = intType "uint8_t"
cPrimitiveType P_Word16 = intType "uint16_t"
cPrimitiveType P_Word32 = intType "uint32_t"
cPrimitiveType P_Word64 = intType "uint64_t"
cPrimitiveType P_Float = return "float"
cPrimitiveType P_Double = return "double"
cPrimitiveType P_ByteVector = return "ByteVector"
cPrimitiveType P_Vector = includeStd ifile "vector" >> return "std::vector"
cPrimitiveType P_String = includeStd ifile "string" >> return "std::string"
cPrimitiveType P_Sink = includeStd ifile "adl/sink.h" >> return "Sink"

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
cPrimitiveDefault P_ByteVector = Nothing
cPrimitiveDefault P_Vector = Nothing
cPrimitiveDefault P_String = Nothing
cPrimitiveDefault P_Sink = Nothing

cPrimitiveLiteral :: PrimitiveType -> JSON.Value -> T.Text
cPrimitiveLiteral P_Void JSON.Null = "Void()"
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
cPrimitiveLiteral P_ByteVector (JSON.String s) = template "ByteVector::fromLiteral($1)" [T.pack (show (decode s))]
  where
    decode s = case B64.decode (T.encodeUtf8 s) of
      (Left _) -> "???"
      (Right s) -> s
cPrimitiveLiteral P_Vector _ = "????" -- never called
cPrimitiveLiteral P_String (JSON.String s) = T.pack (show s)
cPrimitiveLiteral P_Sink _ = "????" -- never called

  
