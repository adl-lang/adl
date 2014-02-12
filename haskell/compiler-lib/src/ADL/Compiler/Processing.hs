{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ADL.Compiler.Processing where

import Prelude

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Exception
import qualified Data.Traversable as T
import System.FilePath(joinPath)
import Data.Ord(comparing)
import Data.List(find,partition,sortBy)
import Data.Foldable(foldMap)
import Data.Monoid
import Data.Maybe(catMaybes)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as JSON

import qualified ADL.Compiler.ParserP as P

import ADL.Utils.Format

import ADL.Compiler.AST
import ADL.Compiler.Primitive
import ADL.Compiler.EIO

type SModule = Module ScopedName
type SModuleMap = Map.Map ModuleName SModule

loadModule :: FilePath -> (ModuleName -> [FilePath]) -> SModuleMap -> EIO T.Text (SModule,SModuleMap)
loadModule fpath0 findm mm = do
    m0 <- parseAndCheckFile fpath0
    mm' <- addDeps m0 mm
    return (m0,mm')
  where
    parseFile :: FilePath -> EIO T.Text (Module0 ScopedName)
    parseFile fpath = mapError (T.pack .show ) $ eioFromEither $ P.fromFile P.moduleFile fpath

    checkDeclarations :: Module0 ScopedName -> EIO T.Text SModule
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

    parseAndCheckFile :: FilePath -> EIO T.Text (Module ScopedName)
    parseAndCheckFile f = parseFile f >>= checkDeclarations                        

    addDeps :: SModule -> SModuleMap -> EIO T.Text SModuleMap
    addDeps m mm = do
       foldM addDep mm (Set.toList (getReferencedModules m))

    addDep :: SModuleMap -> ModuleName -> EIO T.Text SModuleMap
    addDep mm mname = case Map.member mname mm of
        True -> return mm
        False -> do
          m <- findModule mname (findm mname)
          let mm' = Map.insert mname m mm
          addDeps m mm'

    findModule :: ModuleName -> [FilePath] -> EIO T.Text SModule
    findModule mname [] = eioError (template "\"$1\":\nUnable to find module '$2'" [T.pack fpath0,formatText mname] )
    findModule mname (fpath:fpaths) = do
        em <-  liftIO $ try (unEIO (parseAndCheckFile fpath))
        case em of
            (Left (ioe::IOError)) -> findModule mname fpaths
            (Right em) -> eioFromEither (return em)

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

data ResolvedType = RT_Named (ScopedName,Decl ResolvedType)
                  | RT_Param Ident
                  | RT_Primitive PrimitiveType
    deriving (Show)

instance Format ResolvedType where
    format (RT_Named (sn,_)) = format sn
    format (RT_Param i) = format i
    format (RT_Primitive pt) = format pt

type RModule = Module ResolvedType
type RModuleMap = Map.Map ModuleName RModule

type TMap = Map.Map ScopedName ResolvedType

isVoidType :: TypeExpr ResolvedType -> Bool
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
undefinedNames m ns = foldMap checkDecl (m_decls m)
    where
      ns' = namescopeForModule m ns

      checkDecl :: (Decl ScopedName) -> [UndefinedName]
      checkDecl Decl{d_type=Decl_Struct s} = checkFields (withTypeParams (s_typeParams s)) (s_fields s)
      checkDecl Decl{d_type=Decl_Union u} = checkFields  (withTypeParams (u_typeParams u)) (u_fields u)
      checkDecl Decl{d_type=Decl_Typedef t} = checkTypeExpr (withTypeParams (t_typeParams t)) (t_typeExpr t)
      checkDecl Decl{d_type=Decl_Newtype n} = checkTypeExpr (withTypeParams (n_typeParams n)) (n_typeExpr n)

      withTypeParams :: [Ident] -> NameScope
      withTypeParams ids = ns'{ns_typeParams=Set.fromList ids}

      checkFields :: NameScope -> [Field ScopedName] -> [UndefinedName]
      checkFields ns fs = foldMap (checkTypeExpr ns.f_type) fs

      checkTypeExpr :: NameScope -> TypeExpr ScopedName -> [UndefinedName]
      checkTypeExpr ns (TypeExpr t args) = checkScopedName ns t `mappend` foldMap (checkTypeExpr ns) args

      checkScopedName :: NameScope -> ScopedName -> [UndefinedName]
      checkScopedName ns sn = case nlookup ns sn of
          LR_NotFound -> [UndefinedName sn]
          _ -> []

-- Resolve all type references in a module. This assumes that all types
-- are resolvable, ie there are no undefined names
resolveModule :: Module ScopedName -> NameScope -> Module ResolvedType
resolveModule m ns = m{m_decls=Map.map (resolveDecl ns') (m_decls m)}
  where
    ns' = namescopeForModule m ns

    resolveDecl :: NameScope -> Decl ScopedName -> Decl ResolvedType

    resolveDecl ns d@Decl{d_type=Decl_Struct s} = d{d_type=Decl_Struct (s{s_fields=fields'})}
      where
        fields' = resolveFields (withTypeParams ns (s_typeParams s)) (s_fields s)

    resolveDecl ns d@Decl{d_type=Decl_Union u} = d{d_type=Decl_Union (u{u_fields=fields'})}
      where
        fields' = resolveFields (withTypeParams ns (u_typeParams u)) (u_fields u)
                                                
    resolveDecl ns d@Decl{d_type=Decl_Typedef t} = d{d_type=Decl_Typedef (t{t_typeExpr=expr'})}
      where
        expr' = resolveTypeExpr (withTypeParams ns (t_typeParams t)) (t_typeExpr t)

    resolveDecl ns d@Decl{d_type=Decl_Newtype n} = d{d_type=Decl_Newtype (n{n_typeExpr=expr'})}
      where
        expr' = resolveTypeExpr (withTypeParams ns (n_typeParams n)) (n_typeExpr n)

    resolveFields :: NameScope -> [Field ScopedName] -> [Field ResolvedType]
    resolveFields ns fields = [f{f_type=resolveTypeExpr ns (f_type f)} | f <- fields]

    resolveTypeExpr :: NameScope -> TypeExpr ScopedName -> TypeExpr ResolvedType
    resolveTypeExpr ns (TypeExpr t args) = TypeExpr (resolveName ns t) (map (resolveTypeExpr ns) args)

    resolveName :: NameScope -> ScopedName -> ResolvedType
    resolveName ns sn = case nlookup ns sn of
        LR_Defined (LocalDecl decl) -> RT_Named (sn,decl)
        LR_Defined (ImportedDecl mn decl) -> RT_Named (sn{sn_moduleName=mn},decl)
        LR_New decl -> let decl1 = resolveDecl ns1 decl
                           ns1 = ns{ns_locals=Map.insert (sn_name sn) (LocalDecl decl1) (ns_locals ns)}
                       in RT_Named (sn,decl1)
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
      checkTypeCtorApp rt@(RT_Named (_,decl)) expr = check0 rt (declTypeArgCount (d_type decl)) (length expr)

      check0 rt expectedN actualN | expectedN == actualN = mempty
                                  | otherwise = [TypeCtorAppError (rt,expectedN,actualN)]

      declTypeArgCount (Decl_Struct s) = length (s_typeParams s)
      declTypeArgCount (Decl_Union u) = length (u_typeParams u)
      declTypeArgCount (Decl_Typedef t) = length (t_typeParams t)
      declTypeArgCount (Decl_Newtype n) = length (n_typeParams n)


newtype DefaultOverrideError = DefaultOverrideError T.Text

instance Format DefaultOverrideError where
  formatText (DefaultOverrideError t) = t

-- | Check that:
--     * there are no default overrides for parameterised types
--     * the JSON literal for each default override has the appropriate type
--
checkDefaultOverrides :: RModule -> [DefaultOverrideError]
checkDefaultOverrides m = execWriter checkModule
  where
    checkModule :: Writer [DefaultOverrideError] ()
    checkModule = do
      forM_ (Map.elems (m_decls m)) $ \decl -> do
        case decl of
          Decl{d_name=n,d_type=Decl_Struct s} -> checkFields n (s_typeParams s) (s_fields s)
          Decl{d_name=n,d_type=Decl_Union u} -> checkFields n (u_typeParams u) (u_fields u)
          Decl{d_name=n,d_type=Decl_Newtype nt} -> checkType n (n_typeParams nt) (n_typeExpr nt) (n_default nt)
          _ -> return ()

    checkFields :: Ident -> [Ident] -> [Field ResolvedType] -> Writer [DefaultOverrideError] ()
    checkFields n tparams fields = forM_ fields $ \f -> do
      checkType (template "field $1 of $2" [f_name f,n]) tparams (f_type f) (f_default f)

    checkType :: T.Text -> [Ident] -> TypeExpr ResolvedType -> Maybe JSON.Value -> Writer [DefaultOverrideError] ()
    checkType _ _ _ Nothing = return ()
    checkType n tparams te (Just jv) = case validateLiteralForTypeExpr te jv of
      Nothing -> return ()
      (Just err) -> tell [DefaultOverrideError (template "Invalid override of $1: $2" [n,err])]

validateLiteralForTypeExpr :: TypeExpr ResolvedType -> JSON.Value -> Maybe T.Text
validateLiteralForTypeExpr te v = validateTE Map.empty te v
  where
    validateTE m (TypeExpr (RT_Primitive pt) []) v = ptValidateLiteral pt v
    validateTE m (TypeExpr (RT_Primitive P_Vector) [te]) v = vecLiteral m te v
    validateTE m (TypeExpr (RT_Primitive P_Sink) [te]) v = Just "literals not allowed for sinks"
    validateTE m (TypeExpr (RT_Primitive _) _) v =
      error "INTERNAL ERROR: found primitive type with incorrect number of type parameters"

    validateTE m (TypeExpr (RT_Named (sn,decl)) tes) v = case d_type decl of
      (Decl_Struct s) -> structLiteral m s tes v
      (Decl_Union u) -> unionLiteral m u tes v 
      (Decl_Typedef t) -> typedefLiteral m t tes v
      (Decl_Newtype n) -> newtypeLiteral m n tes v
    validateTE m (TypeExpr (RT_Param id) _) v = case Map.lookup id m of
         (Just te) -> validateTE Map.empty te v
         Nothing -> Just "literals not allows for parameterised fields"
    
    vecLiteral m te (JSON.Array v) = case catMaybes errs of
      [] -> Nothing
      (e:_) -> (Just e)
      where
        errs = map (validateTE m te) (V.toList v)
    vecLiteral _ _ _ = Just "expected an array"

    structLiteral m s tes (JSON.Object hm) = HM.foldrWithKey checkField Nothing hm
      where
        checkField :: T.Text -> JSON.Value -> Maybe T.Text -> Maybe T.Text
        checkField k v e@(Just t)= e
        checkField k v Nothing = case find ((k==).f_name) (s_fields s) of
          (Just f) -> validateTE pm (f_type f) v
          Nothing ->
            Just (T.concat ["Field ",k, " in literal doesn't match any in struct definition" ])
        pm = m `Map.union` Map.fromList (zip (s_typeParams s) tes)
    structLiteral m s _ _ = Just "expected an object"

    unionLiteral m u tes (JSON.Object hm) = case HM.toList hm of
      [(k,v)] -> case find ((k==).f_name) (u_fields u) of
        (Just f) -> validateTE pm (f_type f) v
        Nothing ->
          Just (T.concat ["Field ",k, " in literal doesn't match any in union definition" ])
      _ -> Just "literal union must have a single key/value pair"
      where
        pm = m `Map.union` Map.fromList (zip (u_typeParams u) tes)
    unionLiteral m s tes _ = Just "expected an object"

    typedefLiteral m t tes v = validateTE pm (t_typeExpr t) v
      where
        pm = m `Map.union` Map.fromList (zip (t_typeParams t) tes)

    newtypeLiteral m n tes v = validateTE pm (n_typeExpr n) v
      where
        pm = m `Map.union` Map.fromList (zip (n_typeParams n) tes)

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

type ModuleFinder = ModuleName -> [FilePath]

moduleFinder :: [FilePath] -> ModuleFinder
moduleFinder rootpaths (ModuleName mname) =
    [ joinPath ([path]++names++[name0++".adl"]) | path <- rootpaths ]
  where
    names = map T.unpack (init mname)
    name0 = T.unpack (last mname)

loadAndCheckModule :: ModuleFinder -> FilePath -> EIOT RModule
loadAndCheckModule moduleFinder modulePath = do
    (m,mm) <- loadModule1
    mapM_ checkModuleForDuplicates (m:Map.elems mm)
    case sortByDeps (Map.elems mm) of
      Nothing -> eioError "Mutually dependent modules are not allowed"
      (Just mmSorted) -> do  
        ns <- resolveN mmSorted emptyNameScope
        (_,rm) <- resolve1 m ns
        return rm

  where
    loadModule1 :: EIOT (SModule, SModuleMap)
    loadModule1 = loadModule modulePath moduleFinder Map.empty

    checkModuleForDuplicates :: SModule -> EIOT ()
    checkModuleForDuplicates m = case dups of
        [] -> return ()
        _ -> eioError (moduleErrorMessage m (map formatText dups))
      where
        dups = checkDuplicates m

    checkModuleForTypeParamApp :: SModule -> EIOT ()
    checkModuleForTypeParamApp m = return ()

    resolveN :: [SModule] -> NameScope -> EIOT NameScope
    resolveN [] ns = return ns
    resolveN (m:ms) ns = do
        (ns',rm) <- resolve1 m ns
        resolveN ms ns'
    
    resolve1 :: SModule -> NameScope -> EIOT (NameScope,RModule)
    resolve1 m ns = do
        checkUndefined1 m ns
        let rm = resolveModule m ns
            mdecls = Map.mapKeys (\i -> ScopedName (m_name rm) i) (m_decls rm)
            ns' = ns{ns_globals=Map.union (ns_globals ns) mdecls}
        checkTypeCtorApps1 rm
        checkDefaultOverrides1 rm
        return (ns', rm)

    checkUndefined1 :: SModule -> NameScope -> EIOT ()
    checkUndefined1 m ns = case undefinedNames m ns of
        [] -> return ()
        udefs -> eioError (moduleErrorMessage m (map formatText udefs))

    checkTypeCtorApps1 :: RModule -> EIOT ()
    checkTypeCtorApps1 m = case checkTypeCtorApps m of
        [] -> return ()      
        errs -> eioError (moduleErrorMessage m (map formatText errs))

    checkDefaultOverrides1 :: RModule -> EIOT ()
    checkDefaultOverrides1 m = case checkDefaultOverrides m of
        [] -> return ()      
        errs -> eioError (moduleErrorMessage m (map formatText errs))

    moduleErrorMessage m ss = T.intercalate " " ["In module",formatText(m_name m),":\n"] `T.append`
                              T.intercalate "\n  " ss
          
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

-- | eliminate the typedefs from an expression through substitution
expandTypedefs :: TypeExpr ResolvedType -> TypeExpr ResolvedType
expandTypedefs (TypeExpr t ts) = typeExpr t (map expandTypedefs ts)
  where
    typeExpr :: ResolvedType -> [TypeExpr ResolvedType] -> TypeExpr ResolvedType
    typeExpr (RT_Named (_,Decl{d_type=Decl_Typedef t})) ts =
      substTypeParams (Map.fromList (zip (t_typeParams t) ts))
                      (t_typeExpr t)
    typeExpr t ts = TypeExpr t ts

substTypeParams :: Map.Map Ident (TypeExpr ResolvedType) -> TypeExpr ResolvedType -> TypeExpr ResolvedType
substTypeParams m  (TypeExpr (RT_Param n) ts) =
  case Map.lookup n m of
    Nothing -> (TypeExpr (RT_Param n) (map (substTypeParams m) ts))
    Just e -> case ts of
        [] -> e
        _ -> error "BUG: Type param not a concrete type"
substTypeParams m  (TypeExpr t ts) = TypeExpr t (map (substTypeParams m) ts)
