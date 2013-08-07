{-# LANGUAGE OverloadedStrings #-}
module ADL.Compiler.Backends.Cpp(
  generate,
  CppFlags(..)
  ) where

import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as L

import System.Directory(createDirectoryIfMissing)
import System.FilePath(joinPath,takeDirectory)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

import qualified Data.Text as T

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
  format (IncFilePath fp False) = "\"" ++ fp ++ "\""
  format (IncFilePath fp True) = "<" ++ fp ++ ">"

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

-- Write a line of text to the given file
wl :: FileRef -> T.Text -> Gen ()
wl fl t = modify (fl addline)
  where
    addline fs = fs{fs_lines=(fs_indent fs) `T.append` t:fs_lines fs}

-- Write a template to the given file
wt :: FileRef -> T.Text -> [T.Text] -> Gen ()
wt fl pattern args = wl fl (template pattern args)

-- Reference an include file from the given file
include, includeStd :: FileRef -> FilePath -> Gen ()
include fl i = include0 fl (IncFilePath i False)
includeStd fl i = include0 fl (IncFilePath i True)

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

includeModule :: FileRef -> ModuleName -> Gen ()
includeModule fr mn = do
  ms <- get
  let (_,fp2) = ms_fileMapper ms mn
  include fr (fp2 ++ ".h")

mkTemplate :: FileRef -> [Ident] -> Gen ()
mkTemplate _ [] = return ()
mkTemplate fr tps = wt fr "template <$1>"
                    [T.intercalate ", " [T.concat ["class ",cTypeParamName tp] | tp <- tps]]

addMarker :: v -> v -> v -> [a] -> [(v,a)]
addMarker fv v lv as = case add as of
    [] -> []
    ((_,a):as) -> (fv,a):as
  where
    add [] = []
    add [a] = [(lv,a)]
    add (a:as) = (v,a):add as

generateDecl :: Decl ResolvedType -> Gen ()
generateDecl d@(Decl{d_type=(Decl_Struct s)}) = do
  fts <- forM (s_fields s) $ \f -> do
    t <- cTypeExpr (f_type f)
    return (f,t)
  let ctname = cTypeName (d_name d)
  mkTemplate ifile (s_typeParams s)
  wt ifile "struct $1" [ctname]
  wl ifile "{"
  indent ifile $ do
     wt ifile "$1();" [ctname]
     wl ifile ""
     wt ifile "$1(" [ctname]
     indent ifile $ do
       forM_ (addMarker "," "," "" fts) $ \(mark,(f,t)) -> do
          wt ifile "const $1 & $2$3" [t, cFieldName (d_name d) (f_name f),mark]
       wl ifile ");"
     wl ifile ""
     forM_ fts $ \(f,t) -> do
         wt ifile "$1 $2;" [t, cFieldName (d_name d) (f_name f) ]
  wl ifile "};"
                     
generateDecl d@(Decl{d_type=(Decl_Union u)}) = do
  mkTemplate ifile (u_typeParams u)
  wt ifile "struct $1" [cTypeName (d_name d)]
  wl ifile "{"
  wl ifile "   // FIXME: UNION IMPLEMENTATION"
  wl ifile "};"

generateDecl d@(Decl{d_type=(Decl_Typedef t)}) = do
  mkTemplate ifile (t_typeParams t)
  te <- cTypeExpr (t_typeExpr t)
  wt ifile "using $1 = $2;" [cTypeName (d_name d), te]

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
          wl ifile ""
          if hasCustomDefinition n
            then wt ifile "// $1 excluded due to custom definition" [n]
            else generateDecl d

   includeModule cppfile mname
   forM_ (unCppNamespace (ms_moduleMapper ms mname)) $ \i -> do
     wt ifile "namespace $1 {" [i]
     wt cppfile "namespace $1 {" [i]

   mapM_ genDecl sortedDecls

   forM_ (unCppNamespace (ms_moduleMapper ms mname)) $ \_ -> do
     wl cppfile "}"
     wl ifile "}"

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

intType :: T.Text -> Gen T.Text
intType s = include ifile "stdint.h" >> return s

cPrimitiveType :: PrimitiveType -> Gen T.Text
cPrimitiveType P_Void = include ifile "Void.h" >> return "Void"
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
      
customTypes :: Map.Map ScopedName CustomType
customTypes = Map.empty      
  
