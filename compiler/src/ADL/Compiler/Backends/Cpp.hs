{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Cpp(
  generate,
  CppFlags(..)
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L
import Data.Ord (comparing)

import System.Directory(createDirectoryIfMissing)
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

import ADL.Utils.Format
import ADL.Compiler.AST
import ADL.Compiler.EIO
import ADL.Compiler.Processing
import ADL.Compiler.Primitive
import ADL.Compiler.Utils

newtype CppNamespace = CppNamespace { unCppNamespace :: [Ident] }
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
  fs_lines :: [T.Text]
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
   ms_fileMapper :: ModuleName -> (FilePath,FilePath),
   ms_customTypes :: Map.Map ScopedName CustomType,
   ms_incFile :: FState,
   ms_cppFile :: FState
}

data CustomType = CustomType {
   ct_name :: Ident,
   ct_includes :: Set.Set IncFilePath
}

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

isVoidType :: TypeExpr ResolvedType -> Bool
isVoidType (TypeExpr (RT_Primitive P_Void) []) = True
isVoidType _ = False

type TypeBindingMap = Map.Map Ident (TypeExpr ResolvedType)

-- Returns the c++ typer expression corresponding to the
-- given ADL type expression
cTypeExpr :: TypeExpr ResolvedType -> Gen T.Text
cTypeExpr = cTypeExprB Map.empty

cTypeExprB :: TypeBindingMap -> TypeExpr ResolvedType -> Gen T.Text
cTypeExprB m (TypeExpr rt []) = cTypeExprB1 m rt
cTypeExprB m (TypeExpr c args) = do
  ct <- cTypeExprB1 m c
  argst <- mapM (cTypeExprB m) args
  return (T.concat $ [ct, "<"] ++ L.intersperse "," argst ++ ["> "])

cTypeExprB1 :: TypeBindingMap -> ResolvedType -> Gen T.Text
cTypeExprB1 _ (RT_Named (sn,_)) = do
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
        return (cTypeName (sn_name sn))
      False -> do
        -- ADL type defined in an imported module
        let m = sn_moduleName sn
            namespace = ms_moduleMapper ms m
            includefile = ms_fileMapper ms m
        includeModule ifile m
        return (template "$1::$2" [formatText namespace, cTypeName (sn_name sn)])
        
cTypeExprB1 m (RT_Param i) = case Map.lookup i m of
    (Just te) -> cTypeExprB m te
    Nothing -> return (cTypeParamName i)
cTypeExprB1 _ (RT_Primitive pt) = cPrimitiveType pt

-- Returns the c++ name corresponding to the ADL type name
cTypeName :: Ident -> Ident
cTypeName n = n

-- Returns the c++ name corresponding to the ADL type parameter
cTypeParamName :: Ident -> Ident
cTypeParamName n = n

-- Returns the c++ name corresponding to the ADL field name
cFieldName :: Ident -> Ident -> Ident
cFieldName _ n = n

-- Returns the c++ name corresponding to the ADL discriminator name
cUnionAccessorName :: Ident -> Ident -> Ident
cUnionAccessorName _ n = n

-- Returns the c++ name corresponding to the ADL discriminator name
cUnionConstructorName :: Ident -> Ident -> Ident
cUnionConstructorName u n = T.append "mk_" (cUnionAccessorName u n)

-- Returns the c++ name corresponding to the ADL discriminator name
cUnionDiscName :: Ident -> Ident -> Ident
cUnionDiscName u n = T.toUpper (cUnionAccessorName u n)

includeModule :: FileRef -> ModuleName -> Gen ()
includeModule fr mn = do
  ms <- get
  let (_,fp2) = ms_fileMapper ms mn
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

commaSep :: [v]  -> [(v,T.Text)]
commaSep [] = []
commaSep [v] = [(v,"")]
commaSep (v:vs) = (v,","):commaSep vs

generateDecl :: Decl ResolvedType -> Gen ()
generateDecl d@(Decl{d_type=(Decl_Struct s)}) = do
  fts <- forM (s_fields s) $ \f -> do
    t <- cTypeExpr (f_type f)
    litv <- case f_default f of
        (Just v) -> mkLiteral (f_type f) v
        Nothing -> mkDefaultLiteral (f_type f)
    return (cFieldName (d_name d) (f_name f), f, t, litv)
  let ctname = cTypeName (d_name d)
      ctnameP = case s_typeParams s of
        [] -> ctname
        ids -> template "$1<$2>" [ctname,T.intercalate "," ids]

  -- Class Declaration
  let (wl,wt,indent) = writers ifile
  wl ""
  mkTemplate ifile (s_typeParams s)
  wt "struct $1" [ctname]
  wl "{"
  indent  $ do
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
  wl "};"
  wl ""

  -- Associated functions
  mkTemplate ifile (s_typeParams s)
  wt "bool operator<( const $1 &a, const $1 &b );" [ctnameP]
  wl ""
  
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
  wl ""

  -- Non-inline functions
  -- will still end up in header file if it's a template class
  mkTemplate fr (s_typeParams s)
  wl "bool"
  wt "operator<( const $1 &a, const $1 &b )" [ctnameP]
  wl "{"
  indent  $ do
    forM_ fts $ \(fname, f,t,_) -> do
      wt "if( a.$1 < b.$1 ) return true;" [fname]
      wt "if( b.$1 < a.$1 ) return false;" [fname]
    wl "return false;"
  wl "}"

generateDecl d@(Decl{d_type=(Decl_Union u)}) = do
  fts <- forM (u_fields u) $ \f -> do
    t <- cTypeExpr (f_type f)
    litv <- case f_default f of
        (Just v) -> mkLiteral (f_type f) v
        Nothing -> mkDefaultLiteral (f_type f)
    return (f, t, litv)

  let ctname = cTypeName (d_name d)
      ctnameP = case u_typeParams u of
        [] -> ctname
        ids -> template "$1<$2>" [ctname,T.intercalate "," ids]

  -- The class declaration
  let (wl,wt,indent) = writers ifile
  wl ""
  mkTemplate ifile (u_typeParams u)
  wt "class $1" [ctname]
  wl "{"
  wl "public:"
  indent $ do
    wt "$1();" [ctname]
    forM_ fts $ \(f,t,_) -> do
      let ctorName = cUnionConstructorName (d_name d) (f_name f)
      if isVoidType (f_type f)
        then wt "static $1 $2();" [ctnameP, ctorName ] 
        else wt "static $1 $2( const $3 & v );" [ ctnameP, ctorName, t ]

    wl ""
    wt "$1( const $2 & );" [ctname,ctnameP]
    wt "~$1();" [ctname]
    wt "$1 operator=( const $1 & );" [ctnameP]
    wl ""
    wl "enum DiscType"
    wl "{"
    indent $ do
      forM_ (commaSep fts) $ \((f,t,_),sep) -> do
        wt "$1$2" [cUnionDiscName (d_name d) (f_name f), sep]
    wl "};"
    wl ""
    wl "DiscType d() const;"
    forM_ fts $ \(f,t,_) -> do
      wt "$1 & $2() const;" [t,cUnionAccessorName (d_name d) (f_name f)]
    wl ""
  wl "private:"
  indent $ do
    wl "DiscType d_;"
    wl "void *v_;"
    wl "void clear();"
  wl "};"

  -- Associated functions
  mkTemplate ifile (u_typeParams u)
  wt "bool operator<( const $1 &a, const $1 &b );" [ctnameP]

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
  indent $ wt ": d_($1), v_($2)" [cUnionDiscName (d_name d) (f_name f),lv]
  wl "{"
  wl "}"

generateDecl d@(Decl{d_type=(Decl_Typedef t)}) = do
  te <- cTypeExpr (t_typeExpr t)
  mkTemplate ifile (t_typeParams t)
  wline ifile ""
  wtemplate ifile "using $1 = $2;" [cTypeName (d_name d), te]

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

generateModule :: Module ResolvedType -> Gen ()
generateModule m = do
   ms <- get
   let mname = ms_name ms
       hasCustomDefinition n = Map.member (ScopedName mname n) (ms_customTypes ms)

       sortedDecls = case topologicalSort fst (referencedLocalTypes.snd) (Map.toList (m_decls m)) of
         -- FIXME: the topological sort will fail here with mutually recursive types
         -- Need to work out how to generate code in this situation
         Nothing -> error "Unable to sort decls into order due to mutual recursion"
         Just decls -> decls

       genDecl (n,d) = do
          if hasCustomDefinition n
            then wtemplate ifile "// $1 excluded due to custom definition" [n]
            else generateDecl d

   include ifile "adl.h"
   includeModule cppfile mname
   forM_ (unCppNamespace (ms_moduleMapper ms mname)) $ \i -> do
     wtemplate ifile "namespace $1 {" [i]
     wtemplate cppfile "namespace $1 {" [i]

   mapM_ genDecl sortedDecls

   wline cppfile ""
   wline ifile ""
   forM_ (unCppNamespace (ms_moduleMapper ms mname)) $ \_ -> do
     wline cppfile "}"
     wline ifile "}"

-- | Generate the c++ code for an ADL module into files.
-- The call @writeModuleFile uo mNamespace mFile module@ will use
-- @mNamespace@ to map from ADL Modules to C++ namespaces, and @mFile@
-- to map from ADL Modules to actual file system paths. If @uo@ is
-- true, then the output files will not be written if there are
-- existing identical files.
writeModuleFile :: Bool ->
                   (ModuleName -> CppNamespace) ->
                   (ModuleName -> (FilePath,FilePath)) ->
                   Module ResolvedType ->
                   EIO a ()
writeModuleFile noOverwrite mNamespace mFile m = do
  let fs = FState Set.empty "" []
      s0 = MState (m_name m) mNamespace mFile customTypes fs fs
      (fp1,fp2) =  mFile (m_name m)
      s1 = execState (generateModule m) s0
  saveFile (joinPath [fp1,fp2 ++ ".h"]) (fileText (ms_incFile s1))
  saveFile (joinPath [fp1,fp2 ++ ".cpp"]) (fileText (ms_cppFile s1))
  where
    saveFile fpath t = liftIO $ do
      createDirectoryIfMissing True (takeDirectory fpath)
      writeFileIfRequired noOverwrite fpath t

data CppFlags = CppFlags {
  -- directories where we look for ADL files
  cf_searchPath :: [FilePath],
 
  -- the directory to which we write output files
  cf_outputPath :: FilePath,
  
  -- Files containing custom type definitions
  cf_customTypeFiles :: [FilePath],

  -- if true, we only write files when they differ from what's already there
  cf_noOverwrite :: Bool
  }

namespaceGenerator :: ModuleName -> CppNamespace
namespaceGenerator mn = CppNamespace ("ADL":unModuleName mn)

fileGenerator :: FilePath -> ModuleName -> (FilePath,FilePath)
fileGenerator odir mn = (odir, T.unpack (T.intercalate "." (unModuleName mn)))

generate :: CppFlags -> [FilePath] -> EIOT ()
generate cf modulePaths = catchAllExceptions  $ forM_ modulePaths $ \modulePath -> do
  rm <- loadAndCheckModule (moduleFinder (cf_searchPath cf)) modulePath
--  hctypes <- getCustomTypes (cf_customTypeFiles cf)
  writeModuleFile (cf_noOverwrite cf)
                  namespaceGenerator
                  (fileGenerator (cf_outputPath cf))
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
      t <- cTypeExpr te
      return (LDefault t)
    Just l -> do
      ct <- cPrimitiveType pt
      return (LPrimitive ct l)
mkDefaultLiteral te = do
  t <- cTypeExpr te
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
      t <- cTypeExprB m te
      vals <- mapM (mk m te) (V.toList v)
      return (LVector t vals)
      
    mkStruct m te0 d s tes (JSON.Object hm) = do
      t <- cTypeExprB m te0
      fields1 <- forM (s_fields s) $ \f -> do
        case HM.lookup (f_name f) hm of
          Nothing -> mkDefaultLiteral (f_type f) 
          (Just jv) -> mk m2 (f_type f) jv
      return (LCtor t fields1)
      where
        m2 = m `Map.union` Map.fromList (zip (s_typeParams s) tes)
      
    mkUnion  m te0 d u tes (JSON.Object hm) = do
      t <- cTypeExprB m te0
      let [(fname,jv)] = HM.toList hm
          te1 = getTE fname
      lv <- mk m te1 jv
      return (LUnion t (cUnionConstructorName (d_name d) fname) lv)
      where
        getTE fname = case L.find (\f -> f_name f == fname) (u_fields u) of
          Just f -> f_type f
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
cPrimitiveType P_Sink = include ifile "Sink.h" >> return "Sink"

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

customTypes :: Map.Map ScopedName CustomType
customTypes = Map.empty      
  
