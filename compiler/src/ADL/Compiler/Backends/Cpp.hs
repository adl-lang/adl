{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Cpp(
  generate,
  CppFlags(..)
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L
import Data.Ord (comparing)

import System.FilePath(joinPath,takeDirectory)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import Data.Attoparsec.Number

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as B64

import qualified Text.Parsec as P
import qualified ADL.Compiler.ParserP as P

import ADL.Utils.Format
import ADL.Compiler.AST
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

-- The state of a file for which we are accumulating
-- content
data FState = FState {
  fs_includes :: Set.Set IncFilePath,
  fs_indent :: T.Text,
  fs_lines :: [T.Text],
  fs_namespace :: CppNamespace
  }

-- Produce the actual text form of the accumulated file state
fileText ::FState -> T.Text
fileText fs = T.intercalate "\n" lines
  where
    lines = includes ++ [""] ++ body
    includes = [ template "#include $1" [formatText i] | i <- Set.toList (fs_includes fs) ]
    body = reverse (fs_lines fs)

-- | The state capturing all the output being generated.
data MState = MState {
   ms_name :: ModuleName,
   ms_moduleMapper :: ModuleName -> CppNamespace,
   ms_fileMapper :: ModuleName -> FilePath,
   ms_customTypes :: Map.Map ScopedName CustomType,
   ms_incFile :: FState,
   ms_cppFile :: FState
}

data CustomType = CustomType {
   ct_name :: Ident,
   ct_includes :: Set.Set IncFilePath,
   ct_generateOrigADLType :: Maybe Ident
}

type CustomTypeMap = Map.Map ScopedName CustomType

type Gen = State MState

-- Selector function to control which file is being updated.
type FileRef = (FState -> FState) -> MState-> MState

ifile, cppfile :: FileRef
ifile fu ms = ms{ms_incFile=fu (ms_incFile ms)}
cppfile fu ms = ms{ms_cppFile=fu (ms_cppFile ms)}


writers :: FileRef -> (T.Text -> Gen (),
                       T.Text -> [T.Text] -> Gen (),
                       Gen () -> Gen ())
writers fr = (wline fr, wtemplate fr, indent fr)           

-- Set the namespace we are generating code in
-- Returns the existing namespace
--
-- FIXME: It would be better if this returned the current namespace,
-- so that we could implement a withnamespace function and simplify
-- the generation code accordingly. This would require FileRef being a
-- full data-accessor/lens type.
setnamespace :: FileRef -> CppNamespace -> Gen ()
setnamespace fl ns = do modify (fl setns)
  where
    setns fs = fs{fs_lines = ls ++ fs_lines fs,fs_namespace = ns}
      where
        ls | fs_namespace fs == ns = []
           | length ids0 == 0 = open
           | length ids1 == 0 = close
           | otherwise = open ++ [""] ++ close
        close = [ template "$1 // $2" [
                  T.replicate (length ids0) "}",
                  T.intercalate "::" ids0
                  ] ]
        open = [ template "namespace $1 {" [i] | i <- reverse ids1 ]
        ids0 = unCppNamespace (fs_namespace fs)
        ids1 = unCppNamespace ns
  
-- Write a line of text to the given file
wline :: FileRef -> T.Text -> Gen ()
wline fl t = modify (fl addline)
  where
    addline fs = fs{fs_lines=(fs_indent fs) `T.append` t:fs_lines fs}

-- Write a template to the given file
wtemplate :: FileRef -> T.Text -> [T.Text] -> Gen ()
wtemplate fl pattern args = wline fl (template pattern args)

-- Reference an include file from the given file
include, includeStd :: FileRef -> FilePath -> Gen ()
include fr i = include0 fr (IncFilePath i True)
includeStd fr i = include0 fr (IncFilePath i False)

include0 :: FileRef -> IncFilePath -> Gen ()
include0 fl i = modify (fl $ \fs -> fs{fs_includes=Set.insert i (fs_includes fs)})

-- Generate an indented section within the given file
indent :: FileRef -> Gen a -> Gen a
indent fl g = do
    modify (fl $ \fs -> fs{fs_indent=T.append is (fs_indent fs)})
    a <- g
    modify (fl $ \fs -> fs{fs_indent=T.drop (T.length is) (fs_indent fs)})
    return a
  where
    is = "    "

cblock :: FileRef -> Gen a -> Gen a
cblock fr code = do
  wline fr "{"
  a <- indent fr code
  wline fr "}"
  return a

dblock :: FileRef -> Gen a -> Gen a
dblock fr code = do
  wline fr "{"
  a <- indent fr code
  wline fr "};"
  return a


isVoidType :: TypeExpr ResolvedType -> Bool
isVoidType (TypeExpr (RT_Primitive P_Void) []) = True
isVoidType _ = False

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

-- Returns the c++ name corresponding to the ADL type name
cTypeName :: Ident -> Ident
cTypeName n = n

-- Returns the c++ name corresponding to the ADL type parameter
cTypeParamName :: Ident -> Ident
cTypeParamName n = n

-- Returns the c++ name corresponding to the ADL field name
cFieldName :: Ident -> Ident -> Ident
cFieldName _ n = n

-- Returns the c++ name for the accessor function for a union field
cUnionAccessorName :: Decl t -> Field t  -> Ident
cUnionAccessorName _ f = f_name f

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
  let fp2 = ms_fileMapper ms mn
  include fr (fp2 ++ ".h")

mkTemplate :: FileRef -> [Ident] -> Gen ()
mkTemplate _ [] = return ()
mkTemplate fr tps = wtemplate fr "template <$1>"
                    [T.intercalate ", " [T.concat ["class ",cTypeParamName tp] | tp <- tps]]

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
  wline fr ""
  mkTemplate fr tparams
  wtemplate fr "bool operator<( const $1 &a, const $1 &b );" [ctnameP]
  mkTemplate fr tparams
  wtemplate fr "bool operator==( const $1 &a, const $1 &b );" [ctnameP]

serialisationNamespace = CppNamespace ["ADL"]

declareSerialisation fr tparams ms ctnameP = do
  let ns = ms_moduleMapper ms (ms_name ms)
      ctnameP1 = template "$1::$2" [formatText ns,ctnameP]
  wline fr ""
  setnamespace fr serialisationNamespace
  wline fr ""
  case tparams of
    [] -> wline fr "template <>"
    _ -> mkTemplate fr tparams
  wtemplate fr "struct JsonV<$1>" [ctnameP1]
  dblock ifile $ do
    wtemplate fr "static void toJson( JsonWriter &json, const $1 & v );" [ctnameP1]
    wtemplate fr "static void fromJson( $1 &v, JsonReader &json );" [ctnameP1]
  wline fr ""
  setnamespace fr ns

generateDecl :: Ident -> Decl ResolvedType -> Gen ()
generateDecl dn d@(Decl{d_type=(Decl_Struct s)}) = do
  ms <- get
  fts <- forM (s_fields s) $ \f -> do
    t <- cTypeExpr False (f_type f)
    litv <- case f_default f of
        (Just v) -> mkLiteral (f_type f) v
        Nothing -> mkDefaultLiteral (f_type f)
    return (cFieldName dn (f_name f), f, t, litv)
  let ctname = cTypeName dn
      ctnameP = case s_typeParams s of
        [] -> ctname
        ids -> template "$1<$2>" [ctname,T.intercalate "," ids]

  -- Class Declaration
  let (wl,wt,indent) = writers ifile
  wl ""
  mkTemplate ifile (s_typeParams s)
  wt "struct $1" [ctname]
  dblock ifile $ do
     wt "$1();" [ctname]
     wl ""
     wt "$1(" [ctname]
     indent  $ do
       forM_ (commaSep fts) $ \((fname,f,t,_),sep) -> do
          wt "const $1 & $2$3" [t, fname,sep]
       wl ");"
     wl ""
     forM_ fts $ \(fname,_,t,_) -> do
         wt "$1 $2;" [t, fname]

  declareOperators ifile (s_typeParams s) ctnameP
  declareSerialisation ifile (s_typeParams s) ms ctnameP

  -- Constructors
  -- will end up in header file if it's a template class
  let fr = if null (s_typeParams s) then cppfile else ifile
  let (wl,wt,indent) = writers fr
  wl ""
  mkTemplate fr (s_typeParams s)
  wt "$1::$2()" [ctnameP, ctname]
  indent $
    forM_ (addMarker ":" "," "," fts) $ \(mark,(fname,f,t,litv)) -> do
       when (not $ literalIsDefault litv) $
            wt "$1 $2($3)" [mark,fname,literalLValue litv]
  wl "{"
  wl "}"
  wl ""
  mkTemplate fr (s_typeParams s)
  wt "$1::$2(" [ctnameP,ctname]
  indent  $ do
    forM_ (commaSep fts) $ \((fname, f,t,_),sep) -> do
      wt "const $1 & $2_$3" [t,fname,sep]
    wl ")"
    forM_ (addMarker ":" "," "," fts) $ \(mark,(fname, f,t,_)) -> do
      wt "$1 $2($2_)" [mark,fname]
  wl "{"
  wl "}"

  -- Non-inline functions
  -- will still end up in header file if it's a template class
  wl ""
  mkTemplate fr (s_typeParams s)
  wl "bool"
  wt "operator<( const $1 &a, const $1 &b )" [ctnameP]
  cblock fr $ do
    forM_ fts $ \(fname, f,t,_) -> do
      wt "if( a.$1 < b.$1 ) return true;" [fname]
      wt "if( b.$1 < a.$1 ) return false;" [fname]
    wl "return false;"

  wl ""
  mkTemplate fr (s_typeParams s)
  wl "bool"
  wt "operator==( const $1 &a, const $1 &b )" [ctnameP]
  cblock fr $ do 
    wl "return"
    forM_ (sepWithTerm "&&" ";" fts) $ \((fname, f,t,_),sep) -> do
      indent $ wt "a.$1 == b.$1 $2" [fname,sep]

  wl ""
  setnamespace fr serialisationNamespace
  wl ""
  mkTemplate fr (s_typeParams s)
  let ns = ms_moduleMapper ms (ms_name ms)
  wl "void"
  wt "JsonV<$1::$2>::toJson( JsonWriter &json, const $1::$2 & v )" [formatText ns,ctnameP]
  cblock fr $ do
    wl "json.startObject();"
    forM_ fts $ \(fname, f,_,_) -> do
      wt "writeField( json, \"$1\", v.$2 );" [f_name f,fname]
    wl "json.endObject();"
    return ()
  wl ""
  mkTemplate fr (s_typeParams s)
  wl "void"
  wt "JsonV<$1::$2>::fromJson( $1::$2 &v, JsonReader &json )" [formatText ns,ctnameP]
  cblock fr $ do
    wl "match( json, JsonReader::START_OBJECT );"
    wl "while( match0( json, JsonReader::FIELD ) )"
    cblock fr $ do
      forM_ (addMarker "if" "else if" "else if" fts) $ \(ifcmd,(fname, f,_,_)) -> do
        wt "$1( json.fieldName() == \"$2\" )" [ifcmd,f_name f]
        indent $ do
          t <- cTypeExpr True (f_type f)
          wt "JsonV<$1>::fromJson( v.$2, json );" [t,fname]
      wl "else"
      indent $ wl "ignore( json );"
    wl "match( json, JsonReader::END_OBJECT );"

  wl ""
  setnamespace fr ns

generateDecl dn d@(Decl{d_type=(Decl_Union u)}) = do
  ms <- get
  fts <- forM (u_fields u) $ \f -> do
    t <- cTypeExpr False (f_type f)
    litv <- case f_default f of
        (Just v) -> mkLiteral (f_type f) v
        Nothing -> mkDefaultLiteral (f_type f)
    return (f, t, litv)

  let ctname = cTypeName dn
      ctnameP = case u_typeParams u of
        [] -> ctname
        ids -> template "$1<$2>" [ctname,T.intercalate "," ids]

  -- The class declaration
  let (wl,wt,indent) = writers ifile
      fr = ifile
  wl ""
  mkTemplate fr (u_typeParams u)
  wt "class $1" [ctname]
  wl "{"
  wl "public:"
  indent $ do
    wt "$1();" [ctname]
    forM_ fts $ \(f,t,_) -> do
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
    wl "{"
    indent $ do
      forM_ (commaSep fts) $ \((f,t,_),sep) -> do
        wt "$1$2" [cUnionDiscName d f, sep]
    wl "};"
    wl ""
    wl "DiscType d() const;"
    forM_ fts $ \(f,t,_) -> do
      when (not $ isVoidType (f_type f)) $
        wt "$1 & $2() const;" [t,cUnionAccessorName d f]
    wl ""
    forM_ fts $ \(f,t,_) -> do
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
  declareSerialisation ifile (u_typeParams u) ms ctnameP

  wl ""
  mkTemplate fr (u_typeParams u)
  case u_typeParams u of
    [] ->  wt "inline $1::DiscType $1::d() const" [ctnameP]
    _ ->  wt "typename $1::DiscType $1::d() const" [ctnameP]
  cblock fr $ do
    wl "return d_;"

  forM_ fts $ \(f,t,_) -> do
    when (not $ isVoidType (f_type f)) $ do
      wl ""
      mkTemplate fr (u_typeParams u)
      wt "inline $1 & $2::$3() const" [t,ctnameP,cUnionAccessorName d f]
      cblock fr $ do
        wt "if( d_ == $1 )" [cUnionDiscName d f]
        cblock fr $ do
          wt "return *($1 *)p_;" [t]
        wl "throw invalid_union_access();"

  -- Implementation
  -- will end up in header file if it's a template class
  let fr = if null (u_typeParams u) then cppfile else ifile
  let (wl,wt,indent) = writers fr

  wl ""
  mkTemplate fr (u_typeParams u)
  wt "$1::$2()" [ctnameP,ctname]
  -- FIXME :: Confirm that typechecker disallows empty unions, so the
  -- head below is ok.
  let (f,t,litv) = head fts
      lv = if isVoidType (f_type f)
           then "0"
           else template "new $1" [literalPValue litv]
  indent $ wt ": d_($1), p_($2)" [cUnionDiscName d f,lv]
  wl "{"
  wl "}"
  forM_ fts $ \(f,t,_) -> do
    let ctorName = cUnionConstructorName d f
    wl ""
    mkTemplate fr (u_typeParams u)
    if isVoidType (f_type f)
      then do
        wt "$1 $1::$2()" [ctnameP, ctorName ]
        cblock fr $
          wt "return $1( $2, 0 );" [ctnameP, cUnionDiscName d f]
      else do
        wt "$1 $1::$2( const $3 & v )" [ ctnameP, ctorName, t ]
        cblock fr $
          wt "return $1( $2, new $3(v) );" [ctnameP, cUnionDiscName d f,t]
  wl ""
  mkTemplate fr (u_typeParams u)
  wt "$1::$2( const $1 & v )" [ctnameP,ctname]
  indent $ wl ": d_(v.d_), p_(copy(v.d_,v.p_))"
  cblock fr $ return ()
  wl ""
  mkTemplate fr (u_typeParams u)
  wt "$1::~$2()" [ctnameP,ctname]
  cblock fr $ do
    wl "free(d_,p_);"
  wl ""
  mkTemplate fr (u_typeParams u)
  wt "$1 & $1::operator=( const $1 & o )" [ctnameP]
  cblock fr $ do
    wl "free(d_,p_);"
    wl "d_ = o.d_;"
    wl "p_ = copy( o.d_, o.p_ );"

  forM_ fts $ \(f,t,_) -> do
    wl ""
    mkTemplate fr (u_typeParams u)
    if isVoidType (f_type f)
      then do
        wt "void $1::$2()" [ctnameP,cUnionSetterName d f]
        cblock fr $ do
          wt "if( d_ != $1 )" [cUnionDiscName d f]
          cblock fr $ do
            wl "free(d_,p_);"
            wt "d_ = $1;" [cUnionDiscName d f]
            wt "p_ = 0;" [cUnionDiscName d f]
          
      else do
        wt "const $1 & $2::$3(const $1 &v)" [t,ctnameP,cUnionSetterName d f]
        cblock fr $ do
          wt "if( d_ == $1 )" [cUnionDiscName d f]
          cblock fr $ do
            wt "*($1 *)p_ = v;" [t]
          wl "else"
          cblock fr $ do
            wl "free(d_,p_);"
            wt "d_ = $1;" [cUnionDiscName d f]
            wt "p_ = new $1(v);" [t]
          wt "return *($1 *)p_;" [t]

  wl ""
  mkTemplate fr (u_typeParams u)
  wt "$1::$2(DiscType d, void *p)" [ctnameP,ctname]
  indent $ wl ": d_(d), p_(p)"
  cblock fr $ return ()

  wl ""
  mkTemplate fr (u_typeParams u)
  wt "void $1::free(DiscType d, void *p)" [ctnameP]
  cblock fr $ do
    wl "switch( d )"
    cblock fr $ 
      forM_ fts $ \(f,t,_) -> do
        if isVoidType (f_type f)
          then wt "case $1: return;"
               [cUnionDiscName d f]
          else wt "case $1: delete ($2 *)p;"
               [cUnionDiscName d f,t]
  wl ""
  mkTemplate fr (u_typeParams u)
  wt "void * $1::copy( DiscType d, void *p )" [ctnameP]
  cblock fr $ do
    wl "switch( d )"
    cblock fr $ 
      forM_ fts $ \(f,t,_) -> do
        if isVoidType (f_type f)
          then wt "case $1: return 0;"
               [cUnionDiscName d f]
          else wt "case $1: return new $2(*($2 *)p);"
               [cUnionDiscName d f,t]

  wl ""
  mkTemplate fr (u_typeParams u)
  wl "bool"
  wt "operator<( const $1 &a, const $1 &b )" [ctnameP]
  cblock fr $ do
    wl "if( a.d() < b.d() ) return true;"
    wl "if( b.d() < a.d()) return false;"
    wl "switch( a.d() )"
    cblock fr $ 
      forM_ fts $ \(f,t,_) -> do
        if isVoidType (f_type f)
          then wt "case $1::$2: return false;"
               [ctnameP,cUnionDiscName d f]
          else wt "case $1::$2: return a.$3() < b.$3();"
               [ctnameP,cUnionDiscName d f,cUnionAccessorName d f]

  wl ""
  mkTemplate fr (u_typeParams u)
  wl "bool"
  wt "operator==( const $1 &a, const $1 &b )" [ctnameP]
  cblock fr $ do 
    wl "if( a.d() != b.d() ) return false;"
    wl "switch( a.d() )"
    cblock fr $ 
      forM_ fts $ \(f,t,_) -> do
        if isVoidType (f_type f)
          then wt "case $1::$2: return true;"
               [ctnameP,cUnionDiscName d f]
          else wt "case $1::$2: return a.$3() == b.$3();"
               [ctnameP,cUnionDiscName d f,cUnionAccessorName d f]
    

generateDecl dn d@(Decl{d_type=(Decl_Typedef t)}) = do
  te <- cTypeExpr False (t_typeExpr t)
  wline ifile ""
  mkTemplate ifile (t_typeParams t)
  wtemplate ifile "using $1 = $2;" [cTypeName dn, te]

localTypes :: TypeExpr ResolvedType -> Set.Set Ident
localTypes (TypeExpr c args) = Set.unions (localTypes1 c:[localTypes a | a <- args])

localTypes1 :: ResolvedType -> Set.Set Ident
localTypes1 (RT_Named (sn,_)) = case sn_moduleName sn of
    ModuleName [] -> Set.singleton (sn_name sn)
    -- FIXME: need to either check if fully scoped name matches current module here,
    -- or, alternatively, map fully scoped local references to unscoped ones as a compiler
    -- phase.
    _ -> Set.empty 
localTypes1 (RT_Param _) = Set.empty
localTypes1 (RT_Primitive _) = Set.empty

referencedLocalTypes :: Decl ResolvedType -> Set.Set Ident
referencedLocalTypes d = Set.delete (d_name d) (rtypes d)
  where
    rtypes (Decl{d_type=(Decl_Struct s)}) = Set.unions [ localTypes (f_type f) | f <- s_fields s]
    rtypes (Decl{d_type=(Decl_Union u)}) = Set.unions [ localTypes (f_type f) | f <- u_fields u]
    rtypes (Decl{d_type=(Decl_Typedef t)}) = localTypes (t_typeExpr t)
    
generateCustomType :: Ident -> Decl ResolvedType -> CustomType -> Gen ()
generateCustomType n d ct = do
  wline ifile ""
  case ct_generateOrigADLType ct of
    Nothing -> do
      wtemplate ifile "// $1 excluded due to custom definition" [n]
    Just i -> do
      wtemplate ifile "// $1 generated as $2 due to custom definition" [n,i]
      wline ifile ""
      generateDecl i d

generateModule :: Module ResolvedType -> Gen ()
generateModule m = do
   ms <- get
   let mname = ms_name ms
       sortedDecls = case topologicalSort fst (referencedLocalTypes.snd) (Map.toList (m_decls m)) of
         -- FIXME: the topological sort will fail here with mutually recursive types
         -- Need to work out how to generate code in this situation
         Nothing -> error "Unable to sort decls into order due to mutual recursion"
         Just decls -> decls

       genDecl (n,d) = do
          case Map.lookup (ScopedName mname n) (ms_customTypes ms) of
            Nothing -> generateDecl (d_name d) d
            (Just ct) -> generateCustomType n d ct

   include ifile "adl.h"
   includeModule cppfile mname
   setnamespace ifile (ms_moduleMapper ms mname) 
   setnamespace cppfile (ms_moduleMapper ms mname) 

   mapM_ genDecl sortedDecls

   setnamespace ifile (CppNamespace [])
   setnamespace cppfile (CppNamespace [])

-- | Generate the c++ code for an ADL module into files.
-- The call @writeModuleFile uo mNamespace mFile module@ will use
-- @mNamespace@ to map from ADL Modules to C++ namespaces, and @mFile@
-- to map from ADL Modules to actual file system paths. If @uo@ is
-- true, then the output files will not be written if there are
-- existing identical files.
writeModuleFile :: (ModuleName -> CppNamespace) ->
                   (ModuleName -> FilePath) ->
                   CustomTypeMap -> 
                   (FilePath -> T.Text -> IO ()) ->
                   Module ResolvedType ->
                   EIO a ()
writeModuleFile mNamespace mFile customTypes fileWriter m = do
  let fs = FState Set.empty "" [] (CppNamespace [])
      s0 = MState (m_name m) mNamespace mFile customTypes fs fs
      fp =  mFile (m_name m)
      s1 = execState (generateModule m) s0
  liftIO $ fileWriter (fp ++ ".h") (fileText (ms_incFile s1))
  liftIO $ fileWriter (fp ++ ".cpp") (fileText (ms_cppFile s1))

data CppFlags = CppFlags {
  -- directories where we look for ADL files
  cf_searchPath :: [FilePath],
 
  -- Files containing custom type definitions
  cf_customTypeFiles :: [FilePath],

  cf_fileWriter :: FilePath -> T.Text -> IO ()
  }

getCustomTypes :: [FilePath] -> EIOT CustomTypeMap
getCustomTypes fps = fmap Map.unions (mapM get0 fps)
  where
    get0 :: FilePath -> EIOT CustomTypeMap
    get0 fp = do
      mv <- liftIO $ aFromJSONFile jsflags fp
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
        let ct = CustomType (CC.customType_cppname c) includes (CC.customType_generateOrigADLType c)
        return (sn,ct)
      where
        adlname = CC.customType_adlname c
        includes = Set.fromList (map mkInc (CC.customType_cppincludes c))
        mkInc i = IncFilePath (T.unpack (CC.include_name i)) (CC.include_system i)

namespaceGenerator :: ModuleName -> CppNamespace
namespaceGenerator mn = CppNamespace ("ADL":unModuleName mn)

fileGenerator :: ModuleName -> FilePath
fileGenerator mn = T.unpack (T.intercalate "." (unModuleName mn))

generate :: CppFlags -> [FilePath] -> EIOT ()
generate cf modulePaths = catchAllExceptions  $ forM_ modulePaths $ \modulePath -> do
  rm <- loadAndCheckModule (moduleFinder (cf_searchPath cf)) modulePath
  customTypes <- getCustomTypes (cf_customTypeFiles cf)
  writeModuleFile namespaceGenerator
                  fileGenerator
                  customTypes
                  (cf_fileWriter cf)
                  rm
----------------------------------------------------------------------

data Literal = LDefault Ident
             | LCtor Ident [Literal]
             | LUnion Ident T.Text Literal
             | LVector Ident [Literal]
             | LPrimitive Ident T.Text

mkDefaultLiteral :: TypeExpr ResolvedType -> Gen Literal
mkDefaultLiteral te@(TypeExpr (RT_Primitive pt) []) =
  case cPrimitiveDefault pt of
    Nothing -> do
      t <- cTypeExpr False te
      return (LDefault t)
    Just l -> do
      ct <- cPrimitiveType pt
      return (LPrimitive ct l)
mkDefaultLiteral te = do
  t <- cTypeExpr False te
  return (LDefault t)
    

mkLiteral :: TypeExpr ResolvedType -> JSON.Value -> Gen Literal
mkLiteral te jv = mk Map.empty te jv
  where
    mk :: TypeBindingMap -> TypeExpr ResolvedType -> JSON.Value -> Gen Literal
    mk _ (TypeExpr (RT_Primitive pt) []) v = do
      ct <- cPrimitiveType pt
      return (LPrimitive ct (cPrimitiveLiteral pt v))
    mk  m (TypeExpr (RT_Param id) _) v = case Map.lookup id m of
     (Just te) -> mk m te v
     Nothing -> error "Failed to find type binding in mkLiteral"
    mk m (TypeExpr (RT_Primitive P_Vector) [te]) jv = mkVec m te jv
    mk m te0@(TypeExpr (RT_Named (_,decl)) tes) jv = case d_type decl of
      (Decl_Struct s) -> mkStruct m te0 decl s tes jv
      (Decl_Union u) -> mkUnion m te0 decl u tes jv 
      (Decl_Typedef t) -> mkTypedef m decl t tes jv

    mkVec m te (JSON.Array v) = do
      t <- cTypeExprB False m te
      vals <- mapM (mk m te) (V.toList v)
      return (LVector t vals)
      
    mkStruct m te0 d s tes (JSON.Object hm) = do
      t <- cTypeExprB False m te0
      fields1 <- forM (s_fields s) $ \f -> do
        case HM.lookup (f_name f) hm of
          Nothing -> mkDefaultLiteral (f_type f) 
          (Just jv) -> mk m2 (f_type f) jv
      return (LCtor t fields1)
      where
        m2 = m `Map.union` Map.fromList (zip (s_typeParams s) tes)
      
    mkUnion  m te0 d u tes (JSON.Object hm) = do
      t <- cTypeExprB False m te0
      let [(fname,jv)] = HM.toList hm
          f = getF fname
      lv <- mk m (f_type f) jv
      return (LUnion t (cUnionConstructorName d f) lv)
      where
        getF fname = case L.find (\f -> f_name f == fname) (u_fields u) of
          Just f -> f
        m2 = m `Map.union` Map.fromList (zip (u_typeParams u) tes)

    mkTypedef m d t tes v = mk m2 (t_typeExpr t) v
      where
        m2 = m `Map.union` Map.fromList (zip (t_typeParams t) tes)

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

literalIsDefault :: Literal -> Bool
literalIsDefault (LDefault _) = True
literalIsDefault _ = False

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
cPrimitiveType P_ByteVector = includeStd ifile "string" >> return "std::string"
cPrimitiveType P_Vector = includeStd ifile "vector" >> return "std::vector"
cPrimitiveType P_String = includeStd ifile "string" >> return "std::string"
cPrimitiveType P_Sink = return "Sink"

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
cPrimitiveLiteral P_Int8 (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Int16 (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Int32 (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Int64 (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Word8 (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Word16 (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Word32 (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Word64 (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Float (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Float (JSON.Number (D n)) = litNumber n
cPrimitiveLiteral P_Double (JSON.Number (I n)) = litNumber n
cPrimitiveLiteral P_Double (JSON.Number (D n)) = litNumber n
cPrimitiveLiteral P_ByteVector (JSON.String s) = T.pack (show (decode s))
  where
    decode s = case B64.decode (T.encodeUtf8 s) of
      (Left _) -> "???"
      (Right s) -> s
cPrimitiveLiteral P_Vector _ = "????" -- never called
cPrimitiveLiteral P_String (JSON.String s) = T.pack (show s)
cPrimitiveLiteral P_Sink _ = "????" -- never called

litNumber :: (Num a, Ord a, Show a) => a -> T.Text
litNumber x = T.pack (show x)
  
