{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ADL.Compiler.Processing where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Trans.State.Strict
import Control.Exception
import qualified Data.Traversable as T
import System.FilePath(joinPath,replaceExtension)
import System.Directory(doesFileExist)
import Data.Ord(comparing)
import Data.List(find,partition,sortBy)
import Data.Foldable(foldMap)
import Data.Traversable(for)
import Data.Monoid
import Data.Maybe(catMaybes)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Parsec as P
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON
import qualified Data.Scientific as S

import qualified ADL.Compiler.ParserP as P

import ADL.Utils.Format

import ADL.Compiler.AST
import ADL.Compiler.Primitive
import ADL.Compiler.EIO

type SModule = Module ScopedName
type SModuleMap = Map.Map ModuleName SModule

-- | Maps a module name to the possible paths of the corresponding ADL file
type ModuleFinder = ModuleName -> [FilePath]

-- | Map a path of an ADL file to possible paths where we may find extra definitions
-- to merge.
type FileSetGenerator = FilePath -> [FilePath]

-- | Load and parse an adl file and all of its dependencies
loadModule :: (String -> IO ()) -> FilePath -> ModuleFinder -> FileSetGenerator -> SModuleMap -> EIO T.Text (SModule,SModuleMap)
loadModule log fpath0 findm filesetfn mm = do
    m0 <- parseAndCheckFileSet fpath0 
    mm' <- addDeps m0 mm
    return (m0,mm')
  where
    addDeps :: SModule -> SModuleMap -> EIO T.Text SModuleMap
    addDeps m mm = foldM addDep mm (Set.toList (getReferencedModules m))
      where
        addDep mm mname = case Map.member mname mm of
            True -> return mm
            False -> do
              m <- findModule mname (findm mname)
              let mm' = Map.insert mname m mm
              addDeps m mm'

    findModule :: ModuleName -> [FilePath] -> EIO T.Text SModule
    findModule mname [] = eioError (template "\"$1\":\nUnable to find module '$2'" [T.pack fpath0,formatText mname] )
    findModule mname (fpath:fpaths) = do
      exists <- liftIO (doesFileExist fpath)
      if exists
         then parseAndCheckFileSet fpath
         else findModule mname fpaths

    parseAndCheckFileSet :: FilePath -> EIO T.Text (Module ScopedName)
    parseAndCheckFileSet path = do
      extraFiles <- liftIO (filterM doesFileExist (filesetfn path))
      parseAndCheckFile log path extraFiles

-- | Load and parse a single file, applying checks that can be
-- done on that file alone. Extra files can be specified, that must
-- exist and be for the same ADL module. They will be merged into the
-- resulting module.
parseAndCheckFile :: (String -> IO ()) -> FilePath -> [FilePath] -> EIO T.Text (Module ScopedName)
parseAndCheckFile log file extraFiles = do
    m0 <- parseFile file
    m0Extras <- mapM parseFile extraFiles
    m1 <- addDefaultImports <$> mergeModules m0 m0Extras
    m <- mergeAnnotations' m1
    checkDeclarations m
  where
    parseFile :: FilePath -> EIO T.Text (Module0 Decl0)
    parseFile fpath = do
      liftIO (log ("Reading " <> fpath <> "..."))
      mapError (T.pack .show ) $ eioFromEither $ P.fromFile P.moduleFile fpath

    mergeModules :: Module0 Decl0 -> [Module0 Decl0] -> EIO T.Text (Module0 Decl0)
    mergeModules m extrams = case (filter (\em ->m0_name em /= (m0_name m))) extrams of
      [] -> return (foldr merge m extrams)
      badms -> eioError (template "Unable to merge module(s) $1 into module $2"
                         [T.intercalate ", " (map (formatText.m0_name) badms), formatText (m0_name m)])
      where
        merge :: Module0 Decl0 -> Module0 Decl0 -> Module0 Decl0
        merge ma mb = ma{m0_imports=m0_imports ma <> m0_imports mb,m0_decls=m0_decls ma <> m0_decls mb}
        
    checkDeclarations :: Module0 (Decl ScopedName) -> EIO T.Text SModule
    checkDeclarations (Module0 n i decls0) = do
      let declMap = foldr (\d -> Map.insertWith (++)  (d_name d) [d]) Map.empty decls0
      declMap' <- T.mapM checkDeclList declMap
      return (Module  n i declMap')

    -- Ensure that for all the decls associated with a name, either
    --    * we have one unversioned decl
    --    * we have have a consistently versioned set
    checkDeclList :: [Decl ScopedName] -> EIO T.Text (Decl ScopedName)
    checkDeclList ds = case partition hasVersion ds of
        (ds,[]) -> do
          let ds' = sortBy (comparing fst) [ (i,d) | (d@Decl{d_version=Just i}) <- filter hasVersion ds ]
          if and (zipWith (==) (map fst ds') [1,2..])
            then return (snd (last ds'))
            else eioError (T.intercalate " " ["inconsistent version numbers for",d_name (snd (last ds'))])
        ([],[d]) -> return d
        ([],d:_) -> eioError (T.intercalate " " ["multiple definitions for",d_name d])
        (_,d:_) -> eioError (T.intercalate " " ["inconsistent version/unversioned definitions for",d_name d])
      where
        hasVersion Decl{d_version=Nothing} = False
        hasVersion _ = True

    mergeAnnotations' :: Module0 Decl0 -> EIO T.Text (Module0 (Decl ScopedName))
    mergeAnnotations' m0 = do
      let (m,unusedAnnotations) = mergeAnnotations m0
      case unusedAnnotations of
        [] -> return m
        _ -> eioError (template "No declarations for annotation(s): $1" [T.intercalate "," unusedAnnotations])

    addDefaultImports :: Module0 Decl0 -> Module0 Decl0
    addDefaultImports m = m{m0_imports=extraImports<>m0_imports m}
      where
        extraImports | m0_name m `elem` defModules = []
                     | otherwise = map Import_Module defModules
        defModules = [ModuleName ["sys","annotations"]]

data Duplicate = D_Field T.Text Ident Ident
               | D_Param T.Text Ident Ident

instance Format Duplicate where
  format d = case d of
    (D_Field k s f) -> ds ++ "field " ++ T.unpack f ++ " in " ++ T.unpack k ++ " " ++ T.unpack s
    (D_Param k s p) -> ds ++ "type parameter " ++ T.unpack p ++ " in " ++ T.unpack k ++ " " ++ T.unpack s
    where
      ds = "duplicate definition of "

checkDuplicates :: Module t -> [Duplicate]
checkDuplicates m = structErrors ++ unionErrors ++ typedefErrors ++ newtypeErrors
  where
    structErrors = concat [ structErrors1 n s | Decl{d_name=n,d_type=Decl_Struct s} <- Map.elems (m_decls m) ]
    unionErrors = concat [ unionErrors1 n u | Decl{d_name=n,d_type=Decl_Union u} <- Map.elems (m_decls m) ]
    typedefErrors = concat [ typedefErrors1 n t | Decl{d_name=n,d_type=Decl_Typedef t} <- Map.elems (m_decls m) ]
    newtypeErrors = concat [ newtypeErrors1 n t | Decl{d_name=n,d_type=Decl_Newtype t} <- Map.elems (m_decls m) ]

    structErrors1 n s = (map (D_Field "struct" n) . findDuplicates ) [ f_name f | f <- s_fields s ] ++
                        (map (D_Param "struct" n) . findDuplicates ) (s_typeParams s)

    unionErrors1 n u = (map (D_Field "union" n) . findDuplicates ) [ f_name f | f <- u_fields u ] ++
                       (map (D_Param "union" n) . findDuplicates ) (u_typeParams u)

    typedefErrors1 n t = (map (D_Param "type alias" n) . findDuplicates) (t_typeParams t)
    newtypeErrors1 n t = (map (D_Param "newtype" n) . findDuplicates) (n_typeParams t)

    findDuplicates :: [Ident] -> [Ident]
    findDuplicates as = [ a | (a,n) <- Map.toList (foldr (\a -> Map.insertWith' (+) (T.toCaseFold a) 1) Map.empty as),
                          n > (1::Int) ]

type AnnotationMap = (Map.Map (Either Ident (Ident,Ident)) (Map.Map ScopedName Annotation0))
type MAState = State AnnotationMap

-- | Generates a module where the the annotation declarations have been
-- merged into the corresponding type declarations. Also returns the
-- unmerged annotations.
mergeAnnotations :: Module0 Decl0 -> (Module0 (Decl ScopedName), [T.Text])
mergeAnnotations m0 = (m,unusedAnnotation)
  where
    (m,unusedMap) = runState (mergeModule m0) annotationMap

    annotationMap :: AnnotationMap
    annotationMap = foldr addAnnotation Map.empty [ (annKey a, a0_annotationName a, a) | Decl0_Annotation a <- m0_decls m0]
      where
        addAnnotation :: (Either Ident (Ident,Ident),ScopedName, Annotation0) -> AnnotationMap -> AnnotationMap
        addAnnotation (key,name,ann) = Map.insertWith Map.union key (Map.singleton name ann)
        
        annKey ann@Annotation0{a0_fieldName=Just fieldName} = Right (a0_declName ann,fieldName)
        annKey ann = Left (a0_declName ann)

    unusedAnnotation = map annText (Map.keys unusedMap)
      where
        annText (Left decl) = decl
        annText (Right (decl,field)) = template "$1:$2" [decl,field]

    mergeModule :: Module0 Decl0 -> MAState (Module0 (Decl ScopedName))
    mergeModule m0 = do
      decls' <-  mapM mergeDecl [d | Decl0_Decl d <- m0_decls m0]
      return m0{m0_decls=decls'}

    mergeDecl :: Decl ScopedName -> MAState (Decl ScopedName)
    mergeDecl decl = do
      anns <- pullAnnotations (Left (d_name decl))
      let decl1 = decl{d_annotations=insertAnnotations anns (d_annotations decl)}
      declType <- mergeDeclType (d_name decl1) (d_type decl1)
      return decl1{d_type=declType}

    mergeDeclType :: Ident -> DeclType ScopedName -> MAState (DeclType ScopedName)
    mergeDeclType _ dtype@(Decl_Typedef _) = return dtype
    mergeDeclType _ dtype@(Decl_Newtype _) = return dtype
    mergeDeclType declName (Decl_Struct s) = do
      fields' <- mapM (mergeField declName) (s_fields s)
      return (Decl_Struct s{s_fields=fields'})
    mergeDeclType declName (Decl_Union u) = do
      fields' <- mapM (mergeField declName) (u_fields u)
      return (Decl_Union u{u_fields=fields'})

    mergeField :: Ident -> Field ScopedName -> MAState (Field ScopedName)
    mergeField declName field  = do
      anns <- pullAnnotations (Right (declName,f_name field))
      return field{f_annotations=insertAnnotations anns (f_annotations field)}
      
    pullAnnotations :: (Either Ident (Ident,Ident)) -> MAState (Map.Map ScopedName Annotation0)
    pullAnnotations key = do
      map <- get
      case Map.lookup key map of
         Nothing -> return Map.empty
         (Just anns) -> do
           put (Map.delete key map)
           return anns

    insertAnnotations :: Map.Map ScopedName Annotation0 -> Annotations ScopedName -> Annotations ScopedName
    insertAnnotations amap anns0  = foldr insertAnnotation anns0 (Map.elems amap)
    
    insertAnnotation :: Annotation0 -> Annotations ScopedName -> Annotations ScopedName
    insertAnnotation a0 = Map.insert (a0_annotationName a0) (a0_annotationName a0,a0_value a0)
      

data ResolvedTypeT c
  = RT_Named (ScopedName,Decl (ResolvedTypeT c),c)
  | RT_Param Ident
  | RT_Primitive PrimitiveType

type TypeExprRT c = TypeExpr (ResolvedTypeT c)

instance Show c => Show (ResolvedTypeT c) where
    show (RT_Named (sn,_,c)) = show ("RT_Named",sn,c)
    show (RT_Param i) = show ( "RT_Param",i)
    show (RT_Primitive pt) = show ("RT_Primitive",pt)

instance Format (ResolvedTypeT c) where
    format (RT_Named (sn,_,_)) = format sn
    format (RT_Param i) = format i
    format (RT_Primitive pt) = format pt

type ResolvedType = ResolvedTypeT ()

type RModule = Module ResolvedType
type RModuleMap = Map.Map ModuleName RModule

type TMap = Map.Map ScopedName ResolvedType

isPrimitiveType :: TypeExpr ResolvedType -> Bool
isPrimitiveType (TypeExpr (RT_Primitive _) _) = True
isPrimitiveType _ = False

isVoidType :: TypeExpr (ResolvedTypeT a) -> Bool
isVoidType (TypeExpr (RT_Primitive P_Void) []) = True
isVoidType _ = False

-- Naming Scope
    -- Decls in referenced modules (imported and explicitly referenced)
    -- Decls in current modules
    -- Type params for the current object

data LocalDecl = LocalDecl (Decl ResolvedType)
               | ImportedDecl ModuleName (Decl ResolvedType)
  deriving Show            

data NameScope = NameScope {
    ns_globals :: Map.Map ScopedName (Decl ResolvedType),
    ns_locals :: Map.Map Ident LocalDecl,
    ns_currentModule :: Map.Map Ident (Decl ScopedName),
    ns_typeParams :: Set.Set Ident
} deriving Show

data LookupResult = LR_Defined LocalDecl
                  | LR_New (Decl ScopedName)
                  | LR_Primitive PrimitiveType
                  | LR_TypeVar
                  | LR_NotFound

nlookup :: NameScope -> ScopedName -> LookupResult
nlookup ns sn | unModuleName (sn_moduleName sn) == [] = local (sn_name sn)
              | otherwise = global sn
  where
    global sn = case Map.lookup sn (ns_globals ns) of
        (Just decl) -> LR_Defined (ImportedDecl (sn_moduleName sn) decl)
        Nothing -> LR_NotFound

    local ident = case ptFromText ident of
        (Just pt) -> LR_Primitive pt
        Nothing -> case Map.lookup ident (ns_currentModule ns) of
            (Just decl) -> LR_New decl
            Nothing -> case Map.lookup ident (ns_locals ns) of
                (Just localDecl) -> LR_Defined localDecl
                Nothing -> if Set.member ident (ns_typeParams ns) then LR_TypeVar else LR_NotFound

newtype UndefinedName = UndefinedName ScopedName

instance Format UndefinedName where
  formatText (UndefinedName sn) = T.intercalate " " ["undefined type", formatText sn]

undefinedNames :: Module ScopedName -> NameScope -> [UndefinedName]
undefinedNames m ns0 = foldMap checkDecl (m_decls m)
    where
      ns = namescopeForModule m ns0

      checkDeclType :: (DeclType ScopedName) -> [UndefinedName]
      checkDeclType (Decl_Struct s) = checkFields (withTypeParams (s_typeParams s)) (s_fields s)
      checkDeclType (Decl_Union u) = checkFields  (withTypeParams (u_typeParams u)) (u_fields u)
      checkDeclType (Decl_Typedef t) = checkTypeExpr (withTypeParams (t_typeParams t)) (t_typeExpr t)
      checkDeclType (Decl_Newtype n) = checkTypeExpr (withTypeParams (n_typeParams n)) (n_typeExpr n)

      checkDecl :: (Decl ScopedName) -> [UndefinedName]
      checkDecl decl = checkDeclType (d_type decl) <> checkAnnotations (d_annotations decl)

      withTypeParams :: [Ident] -> NameScope
      withTypeParams ids = ns{ns_typeParams=Set.fromList ids}

      checkField  :: NameScope -> Field ScopedName -> [UndefinedName]
      checkField ns field = checkTypeExpr ns (f_type field) <> checkAnnotations (f_annotations field)

      checkFields :: NameScope -> [Field ScopedName] -> [UndefinedName]
      checkFields ns fs = foldMap (checkField ns) fs

      checkTypeExpr :: NameScope -> TypeExpr ScopedName -> [UndefinedName]
      checkTypeExpr ns (TypeExpr t args) = checkScopedName ns t `mappend` foldMap (checkTypeExpr ns) args

      checkScopedName :: NameScope -> ScopedName -> [UndefinedName]
      checkScopedName ns sn = case nlookup ns sn of
          LR_NotFound -> [UndefinedName sn]
          _ -> []

      checkAnnotations :: Annotations ScopedName -> [UndefinedName]
      checkAnnotations as = concat [ checkScopedName ns n | (n,(_,_)) <- (Map.toList as)]

-- Resolve all type references in a module. This assumes that all types
-- are resolvable, ie there are no undefined names
resolveModule :: Module ScopedName -> NameScope -> Module ResolvedType
resolveModule m ns0 = m{m_decls=Map.map (resolveDecl ns) (m_decls m)}
  where
    ns = namescopeForModule m ns0

    resolveDeclType :: NameScope -> DeclType ScopedName -> DeclType ResolvedType
    resolveDeclType ns (Decl_Struct s) = Decl_Struct (s{s_fields=fields'})
      where
        fields' = resolveFields (withTypeParams ns (s_typeParams s)) (s_fields s)
    resolveDeclType ns (Decl_Union u) = Decl_Union (u{u_fields=fields'})
      where
        fields' = resolveFields (withTypeParams ns (u_typeParams u)) (u_fields u)
    resolveDeclType ns (Decl_Typedef t) = Decl_Typedef (t{t_typeExpr=expr'})
      where
        expr' = resolveTypeExpr (withTypeParams ns (t_typeParams t)) (t_typeExpr t)
    resolveDeclType ns (Decl_Newtype n) = Decl_Newtype (n{n_typeExpr=expr'})
      where
        expr' = resolveTypeExpr (withTypeParams ns (n_typeParams n)) (n_typeExpr n)

    resolveDecl :: NameScope -> Decl ScopedName -> Decl ResolvedType
    resolveDecl ns decl = decl
      { d_annotations=resolveAnnotations ns (d_annotations decl)
      , d_type=resolveDeclType ns (d_type decl)
      }

    resolveAnnotations :: NameScope -> Annotations ScopedName -> Annotations ResolvedType
    resolveAnnotations nscope = Map.fromList . map resolve1 . Map.elems
       where
         resolve1 :: (ScopedName,JSON.Value) -> (ScopedName,(ResolvedType,JSON.Value))
         resolve1 (sn,jv) = (sn',(rt,jv))
           where
             rt = resolveName nscope sn
             sn' = case rt of 
                 RT_Named (sn',_,_) -> sn'
                 _ -> error "BUG: Unabled to resolve annotation to a declaration"

    resolveField :: NameScope -> Field ScopedName -> Field ResolvedType
    resolveField ns field = field
      { f_type=resolveTypeExpr ns (f_type field)
      , f_annotations=resolveAnnotations ns (f_annotations field)
      }

    resolveFields :: NameScope -> [Field ScopedName] -> [Field ResolvedType]
    resolveFields ns fields = map (resolveField ns) fields

    resolveTypeExpr :: NameScope -> TypeExpr ScopedName -> TypeExpr ResolvedType
    resolveTypeExpr ns (TypeExpr t args) = TypeExpr (resolveName ns t) (map (resolveTypeExpr ns) args)

    resolveName :: NameScope -> ScopedName -> ResolvedType
    resolveName ns sn = case nlookup ns sn of
        LR_Defined (LocalDecl decl) -> RT_Named (sn,decl,())
        LR_Defined (ImportedDecl mn decl) -> RT_Named (sn{sn_moduleName=mn},decl,())
        LR_New decl -> let decl1 = resolveDecl ns1 decl
                           ns1 = ns{ns_locals=Map.insert (sn_name sn) (LocalDecl decl1) (ns_locals ns)}
                       in RT_Named (sn,decl1,())
        LR_TypeVar -> RT_Param (sn_name sn)
        LR_Primitive pt -> RT_Primitive pt
        LR_NotFound -> error ("PRECONDITION FAIL: unable to resolve type for " ++ show sn)

    withTypeParams :: NameScope -> [Ident] -> NameScope
    withTypeParams ns ids = ns{ns_typeParams=Set.fromList ids}

-- | Check that the all applications of type constructors are passed the
-- correct number of parameters
newtype TypeCtorAppError = TypeCtorAppError (ResolvedType,Int,Int)

instance Format TypeCtorAppError where
  formatText (TypeCtorAppError (s,0,a)) =
    T.intercalate " " ["type",formatText s,"doesn't take arguments"]
  formatText (TypeCtorAppError (s,e,a)) =
    T.intercalate " " ["type constructor",formatText s,"expected" ,fshow e,"arguments, but was passed",fshow a]

checkTypeCtorApps :: RModule -> [TypeCtorAppError]
checkTypeCtorApps m = foldMap checkDecl (m_decls m)
  where
      checkDecl :: (Decl ResolvedType) -> [TypeCtorAppError]
      checkDecl Decl{d_type=Decl_Struct s} = checkFields (s_fields s)
      checkDecl Decl{d_type=Decl_Union u} = checkFields (u_fields u)
      checkDecl Decl{d_type=Decl_Typedef t} = checkTypeExpr (t_typeExpr t)
      checkDecl Decl{d_type=Decl_Newtype n} = checkTypeExpr (n_typeExpr n)

      checkFields :: [Field ResolvedType] -> [TypeCtorAppError]
      checkFields fs = foldMap (checkTypeExpr . f_type) fs

      checkTypeExpr :: TypeExpr ResolvedType -> [TypeCtorAppError]
      checkTypeExpr (TypeExpr t exprs) = checkTypeCtorApp t exprs `mappend` foldMap checkTypeExpr exprs

      checkTypeCtorApp :: ResolvedType -> [TypeExpr ResolvedType] -> [TypeCtorAppError]
      checkTypeCtorApp (RT_Param _) [] = mempty
      checkTypeCtorApp rt@(RT_Param _) expr = [TypeCtorAppError (rt,0,length expr)]
      checkTypeCtorApp rt@(RT_Primitive pt) expr = check0 rt (ptArgCount pt) (length expr)
      checkTypeCtorApp rt@(RT_Named (_,decl,())) expr = check0 rt (declTypeArgCount (d_type decl)) (length expr)

      check0 rt expectedN actualN | expectedN == actualN = mempty
                                  | otherwise = [TypeCtorAppError (rt,expectedN,actualN)]

      declTypeArgCount (Decl_Struct s) = length (s_typeParams s)
      declTypeArgCount (Decl_Union u) = length (u_typeParams u)
      declTypeArgCount (Decl_Typedef t) = length (t_typeParams t)
      declTypeArgCount (Decl_Newtype n) = length (n_typeParams n)


newtype GeneralError = GeneralError T.Text

instance Format GeneralError where
  formatText (GeneralError t) = t

-- | Check that:
--     * there are no default overrides for parameterised types
--     * the JSON literal for each default override has the appropriate type
--
checkDefaultOverrides :: RModule -> [GeneralError]
checkDefaultOverrides m = execWriter checkModule
  where
    checkModule :: Writer [GeneralError] ()
    checkModule = do
      forM_ (Map.elems (m_decls m)) $ \decl -> do
        case decl of
          Decl{d_name=n,d_type=Decl_Struct s} -> checkFields n (s_typeParams s) (s_fields s)
          Decl{d_name=n,d_type=Decl_Union u} -> checkFields n (u_typeParams u) (u_fields u)
          Decl{d_name=n,d_type=Decl_Newtype nt} -> checkType n (n_typeParams nt) (n_typeExpr nt) (n_default nt)
          _ -> return ()

    checkFields :: Ident -> [Ident] -> [Field ResolvedType] -> Writer [GeneralError] ()
    checkFields n tparams fields = forM_ fields $ \f -> do
      checkType (template "field $1 of $2" [f_name f,n]) tparams (f_type f) (f_default f)

    checkType :: T.Text -> [Ident] -> TypeExpr ResolvedType -> Maybe JSON.Value -> Writer [GeneralError] ()
    checkType _ _ _ Nothing = return ()
    checkType n tparams te (Just jv) = case literalForTypeExpr te jv of
      Right _ -> return ()
      Left err -> tell [GeneralError (template "Invalid override of $1: $2" [n,err])]

checkModuleAnnotations :: RModule -> [GeneralError]
checkModuleAnnotations m = execWriter checkModule
  where
    checkModule :: Writer [GeneralError] ()
    checkModule = do
      forM_ (Map.elems (m_decls m)) $ \decl -> do
        checkAnnotations (d_name decl) (d_annotations decl)
        case decl of
          Decl{d_name=n,d_type=Decl_Struct s} -> checkFields n (s_fields s)
          Decl{d_name=n,d_type=Decl_Union u} -> checkFields n (u_fields u)
          _ -> return ()

    checkFields :: Ident -> [Field ResolvedType] -> Writer [GeneralError] ()
    checkFields n fields = forM_ fields $ \f -> do
      checkAnnotations (template "field $1 of $2" [f_name f,n]) (f_annotations f)

    checkAnnotations :: T.Text -> Annotations ResolvedType -> Writer [GeneralError] ()
    checkAnnotations n as = forM_ (Map.toList as) $ \(sn,(rt,jv)) -> do
      case literalForTypeExpr (TypeExpr rt []) jv of
        Right _ -> return ()
        Left err -> tell [GeneralError (template "Invalid literal for annotation $1 of $2: $3" [formatText sn,n,err])]

-- | Represent the construction of a literal value in a
-- language independent way

data Literal te
  = LDefault te
  | LCtor te [Literal te]
  | LUnion te Ident (Literal te)
  | LVector te [Literal te]
  | LStringMap te (Map.Map T.Text (Literal te))
  | LPrimitive PrimitiveType JSON.Value
  deriving (Show)

-- | Generate a `Literal` value from a JSON value.
literalForTypeExpr :: TypeExprRT c -> JSON.Value -> Either T.Text (Literal (TypeExprRT c))
literalForTypeExpr te v = litForTE Map.empty te v
  where
    litForTE m (TypeExpr (RT_Primitive pt) []) v = case ptValidateLiteral pt v of
      Nothing -> Right (LPrimitive pt v)
      Just err -> Left err
    litForTE m te@(TypeExpr (RT_Primitive P_Vector) [te0]) v = LVector te <$> vecLiteral m te0 v
    litForTE m te@(TypeExpr (RT_Primitive P_StringMap) [te0]) v = LStringMap te <$> stringMapLiteral m te0 v
    litForTE m (TypeExpr (RT_Primitive P_Sink) [te]) v = Left "literals not allowed for sinks"
    litForTE m (TypeExpr (RT_Primitive _) _) v =
      error "BUG: found primitive type with incorrect number of type parameters"

    litForTE m te@(TypeExpr (RT_Named (_,decl,_)) tes) v = case d_type decl of
      (Decl_Struct s) -> LCtor te <$> structFields m decl s tes v
      (Decl_Union u) -> uncurry (LUnion te) <$> unionField m decl u tes v
      (Decl_Typedef t) -> typedefLiteral m t tes v
      (Decl_Newtype n) -> (\t ->LCtor te [t]) <$> newtypeLiteral m n tes v
      
    litForTE m (TypeExpr (RT_Param id) _) v = case Map.lookup id m of
      (Just te) -> litForTE Map.empty te v
      Nothing -> Left "literals not allows for parameterised fields"
    
    vecLiteral m te (JSON.Array v) = mapM (litForTE m te) (V.toList v)
    vecLiteral _ _ _ = Left "expected an array"

    stringMapLiteral m te (JSON.Object o) = Map.fromList <$> mapM mkItem (HM.toList o)
      where
         mkItem (key,jv) = do
           lit <- litForTE m te jv
           return (key,lit)
    stringMapLiteral _ _ _ = Left "expected an object"

    structFields m decl s tes (JSON.Object hm) = for (s_fields s) $ \f -> do
      pm <- createParamMap (s_typeParams s) tes m
      ftype <- substTypeParams pm (f_type f)
      case HM.lookup (f_name f) hm of
       (Just jv) -> litForTE pm ftype jv
       Nothing -> case f_default f of
         (Just jv) -> litForTE pm ftype jv
         Nothing -> Right (LDefault ftype)
    structFields _ _ _ _ _ = Left "expected an object"

    unionField m decl u tes (JSON.Object hm) = do
      pm <- createParamMap (u_typeParams u) tes m
      case HM.toList hm of
        [(k,v)] -> case find ((k==).f_name) (u_fields u) of
          (Just f) -> do
            ftype <- substTypeParams pm (f_type f)
            lit <- litForTE pm ftype v
            return (k,lit)
          Nothing ->
            Left (T.concat ["Field ",k, " in literal doesn't match any in union definition for", d_name decl])
        _ -> Left "literal union must have a single key/value pair"
    unionField _ _ _ _ _ = Left "expected an object"

    typedefLiteral m t tes v = do
      pm <- createParamMap (t_typeParams t) tes m
      te <- substTypeParams pm (t_typeExpr t)
      litForTE pm te v

    newtypeLiteral m n tes v = do
      pm <- createParamMap (n_typeParams n) tes m
      te <- substTypeParams pm (n_typeExpr n)
      litForTE pm te v

    createParamMap :: [Ident] -> [TypeExprRT c] -> Map.Map Ident (TypeExprRT c) -> Either T.Text (Map.Map Ident (TypeExprRT c))
    createParamMap typeParams tes m = do
      items <- for (zip typeParams tes) $ \(id,te) -> do
        te' <- substTypeParams m te
        return (id,te')
      return (Map.fromList items)

litNumber :: S.Scientific -> T.Text
litNumber n = T.pack s
  where
   s = case S.floatingOrInteger n of
     (Left r) -> show n
     (Right i) -> show (i::Integer)


namescopeForModule :: Module ScopedName -> NameScope -> NameScope
namescopeForModule m ns = ns
    { ns_locals = Map.fromList [ (sn_name sn,ImportedDecl (sn_moduleName sn) d)
                               | (sn,d)<- Map.toList (ns_globals ns),
                                 any (nameInScope sn) (m_imports m) ]
    , ns_currentModule=m_decls m
    }
  where
    nameInScope sn (Import_ScopedName sn') = sn == sn'
    nameInScope sn (Import_Module m) = (sn_moduleName sn) == m

moduleFinder :: [FilePath] -> ModuleFinder
moduleFinder rootpaths (ModuleName mname) =
    [ joinPath ([path]++names++[name0++".adl"]) | path <- rootpaths ]
  where
    names = map T.unpack (init mname)
    name0 = T.unpack (last mname)

-- | Generate the sets of files to be loaded based upon a
-- list of path extensions.
fileSetGenerator :: [String] -> FileSetGenerator
fileSetGenerator extensions path = map (\ext -> replaceExtension path ext) extensions

data AdlFlags = AdlFlags {
  af_searchPath :: [FilePath],
  af_mergeFileExtensions :: [String],
  af_log :: String -> IO ()
}

defaultAdlFlags :: AdlFlags
defaultAdlFlags = AdlFlags
  { af_searchPath = []
  , af_log = const (return ())
  , af_mergeFileExtensions = ["adl-hs", "adl-java"]
  }
  
loadAndCheckModule :: AdlFlags -> FilePath -> EIOT RModule
loadAndCheckModule af modulePath = do
    (m,mm) <- loadModule1
    mapM_ checkModuleForDuplicates (m:Map.elems mm)
    case sortByDeps (Map.elems mm) of
      Nothing -> eioError (template "Mutually dependent modules are not allowed: $1" [T.intercalate ", " (map formatText (Map.keys mm))])
      (Just mmSorted) -> do  
        ns <- resolveN mmSorted emptyNameScope
        (_,rm) <- resolve1 m ns
        return rm

  where
    loadModule1 :: EIOT (SModule, SModuleMap)
    loadModule1 = loadModule (af_log af) modulePath (moduleFinder (af_searchPath af)) (fileSetGenerator (af_mergeFileExtensions af)) Map.empty

    checkModuleForDuplicates :: SModule -> EIOT ()
    checkModuleForDuplicates m = failOnErrors m (checkDuplicates m)

    resolveN :: [SModule] -> NameScope -> EIOT NameScope
    resolveN [] ns = return ns
    resolveN (m:ms) ns = do
        (ns',rm) <- resolve1 m ns
        resolveN ms ns'
    
    resolve1 :: SModule -> NameScope -> EIOT (NameScope,RModule)
    resolve1 m ns = do
        failOnErrors m (undefinedNames m ns)
        let rm = resolveModule m ns
        failOnErrors rm (checkTypeCtorApps rm)
        failOnErrors rm (checkDefaultOverrides rm)
        failOnErrors rm (checkModuleAnnotations rm)

        let mdecls = Map.mapKeys (\i -> ScopedName (m_name rm) i) (fmap (mapDecl (fullyScopedType (m_name m))) (m_decls rm))
            ns' = ns{ns_globals=Map.union (ns_globals ns) mdecls}
        return (ns', rm)

    failOnErrors :: Format a => Module b -> [a] -> EIOT ()
    failOnErrors _ [] = return ()
    failOnErrors m errs = eioError mesg
      where
        mesg = T.intercalate " " ["In module",formatText(m_name m),":\n"] `T.append`
               T.intercalate "\n  " (map formatText errs)

    emptyNameScope = NameScope Map.empty Map.empty Map.empty Set.empty

sortByDeps :: [SModule] -> Maybe [SModule]
sortByDeps ms = topologicalSort m_name getReferencedModules ms

-- | Sort a list topologically, given a function idf to label each element, and
-- a function depf to calculate the elements dependencies. If the implied graph is not
-- acyclic, then a topological sort is not possible and Nothing is returned.
topologicalSort :: forall a b . (Ord b) => (a->b) -> (a->Set.Set b) -> [a] ->  Maybe [a]
topologicalSort idf depf as = fmap (map fst) (sort1 (addDeps as) Set.empty)
  where
    sort1 :: (Ord b) => [(a, Set.Set b)] -> Set.Set b -> Maybe [(a, Set.Set b)]
    sort1 [] _ = Just []
    sort1 as sofar | length todo == length as = Nothing
                   | otherwise = sort1 todo sofar' >>= \vs -> Just (ok ++ vs)
      where                                                              
        (ok,todo) = partition (\(a,ad)-> Set.null (ad `Set.difference` sofar)) as
        sofar' = Set.unions (sofar:map (Set.singleton . idf . fst) ok)

    addDeps as = map (\a -> (a,depf a)) as

-- | expand all of the typedefs in a module
expandModuleTypedefs :: Module ResolvedType -> Module ResolvedType
expandModuleTypedefs = exModule
  where
  exModule mod@Module{m_decls=ds} = mod{m_decls=fmap exDecl ds}
  exDecl decl@Decl{d_type=d} = decl{d_type=exDeclType d}
  exDeclType (Decl_Struct s) = Decl_Struct (exStruct s)
  exDeclType (Decl_Union u) = Decl_Union (exUnion u)
  exDeclType (Decl_Typedef t) = Decl_Typedef (exTypedef t)
  exDeclType (Decl_Newtype n) = Decl_Newtype (exNewtype n)
  exStruct struct@Struct{s_fields=fs} = struct{s_fields=fmap exField fs}
  exUnion union@Union{u_fields=fs} = union{u_fields=fmap exField fs}
  exTypedef typedef@Typedef{t_typeExpr=t} = typedef{t_typeExpr=expandTypedefs t}
  exNewtype ntype@Newtype{n_typeExpr=t} = ntype{n_typeExpr=expandTypedefs t}
  exField field@Field{f_type=t} = field{f_type=expandTypedefs t}

-- | Remove all typedef declations from a module
removeModuleTypedefs :: Module ResolvedType -> Module ResolvedType
removeModuleTypedefs mod@Module{m_decls=ds} = mod{m_decls=Map.filter (not . isTypedef) ds}
  where
    isTypedef Decl{d_type=Decl_Typedef _} = True
    isTypedef _ = False
  
-- | eliminate the typedefs from an expression through substitution
expandTypedefs :: TypeExpr ResolvedType -> TypeExpr ResolvedType
expandTypedefs (TypeExpr t ts) = typeExpr t (map expandTypedefs ts)
  where
    typeExpr :: ResolvedType -> [TypeExpr ResolvedType] -> TypeExpr ResolvedType
    typeExpr (RT_Named (_,Decl{d_type=Decl_Typedef t},())) ts =
      case substTypeParams (Map.fromList (zip (t_typeParams t) ts)) (t_typeExpr t) of
        Left err -> error ("BUG: " ++ T.unpack err)
        Right te -> te
    typeExpr t ts = TypeExpr t ts

substTypeParams :: Map.Map Ident (TypeExprRT c) -> TypeExprRT c -> Either T.Text (TypeExprRT c)
substTypeParams m  (TypeExpr (RT_Param n) ts) =
  case Map.lookup n m of
    Nothing -> TypeExpr (RT_Param n) <$> mapM (substTypeParams m) ts
    Just e -> case ts of
        [] -> Right e
        _ -> Left "Type param not a concrete type"
substTypeParams m  (TypeExpr t ts) = TypeExpr t <$> mapM (substTypeParams m) ts

fullyScopedName :: ModuleName -> ScopedName -> ScopedName
fullyScopedName mname (ScopedName (ModuleName []) n) = ScopedName mname n
fullyScopedName _ sn = sn

fullyScopedType :: ModuleName -> ResolvedTypeT c -> ResolvedTypeT c
fullyScopedType mname (RT_Named (sn,decl,c)) = RT_Named (fullyScopedName mname sn,mapDecl (fullyScopedType mname) decl,c)
fullyScopedType _ rt = rt

