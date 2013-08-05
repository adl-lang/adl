{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ADL.Compiler.Processing where


import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import System.FilePath(joinPath)
import Data.List(find,partition)
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
    m0 <- parseFile fpath0
    mm' <- addDeps m0 mm
    return (m0,mm')
  where
    parseFile :: FilePath -> EIO T.Text SModule
    parseFile fpath = mapError (T.pack .show ) $ eioFromEither $ P.fromFile P.moduleFile fpath

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
        em <-  liftIO $ try (unEIO (parseFile fpath))
        case em of
            (Left (ioe::IOError)) -> findModule mname fpaths
            (Right em) -> eioFromEither (return em)

data Duplicate = D_StructField Ident Ident
               | D_StructParam Ident Ident
               | D_UnionField Ident Ident
               | D_UnionParam Ident Ident
               | D_Decl Ident

instance Format Duplicate where
  format d = case d of
    (D_Decl n) -> ds ++ T.unpack n
    (D_StructField s f) -> ds ++ "field " ++ T.unpack f ++ " in struct " ++ T.unpack s
    (D_StructParam s p) -> ds ++ "type parameter " ++ T.unpack p ++ " in struct " ++ T.unpack s
    (D_UnionField u f) -> ds ++ "field " ++ T.unpack f ++ " in union " ++ T.unpack u
    (D_UnionParam u p) -> ds ++ "type parameter " ++ T.unpack p ++ " in union " ++ T.unpack u
    where
      ds = "duplicate definition of "

checkDuplicates :: Module t -> [Duplicate]
checkDuplicates m = declErrors ++ structErrors ++ unionErrors
  where
    declErrors = map D_Decl $ findDuplicates [ d_name d | d <- Map.elems (m_decls m) ]
    structErrors = concat [ structErrors1 n s | Decl{d_name=n,d_type=Decl_Struct s} <- Map.elems (m_decls m) ]
    unionErrors = concat [ unionErrors1 n u | Decl{d_name=n,d_type=Decl_Union u} <- Map.elems (m_decls m) ]

    structErrors1 n s = (map (D_StructField n) . findDuplicates ) [ f_name f | f <- s_fields s ] ++
                        (map (D_StructParam n) . findDuplicates ) [ t | t <- s_typeParams s ]

    unionErrors1 n u = (map (D_UnionField n) . findDuplicates ) [ f_name f | f <- u_fields u ] ++
                       (map (D_UnionParam n) . findDuplicates ) [t | t <- u_typeParams u ]

    findDuplicates :: [Ident] -> [Ident]
    findDuplicates as = [ a | (a,n) <- Map.toList (foldr (\a -> Map.insertWith' (+) (T.toCaseFold a) 1) Map.empty as),
                          n > 1 ]

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

      checkFields :: [Field ResolvedType] -> [TypeCtorAppError]
      checkFields fs = foldMap (checkTypeExpr . f_type) fs

      checkTypeExpr :: TypeExpr ResolvedType -> [TypeCtorAppError]
      checkTypeExpr (TypeExpr t exprs) = checkTypeCtorApp t exprs `mappend` foldMap checkTypeExpr exprs

      checkTypeCtorApp :: ResolvedType -> [TypeExpr ResolvedType] -> [TypeCtorAppError]
      checkTypeCtorApp (RT_Param _) _ = mempty -- can't check this
      checkTypeCtorApp rt@(RT_Primitive pt) expr = check0 rt (ptArgCount pt) (length expr)
      checkTypeCtorApp rt@(RT_Named (_,decl)) expr = check0 rt (declTypeArgCount (d_type decl)) (length expr)

      check0 rt expectedN actualN | expectedN == actualN = mempty
                                  | otherwise = [TypeCtorAppError (rt,expectedN,actualN)]

      declTypeArgCount (Decl_Struct s) = length (s_typeParams s)
      declTypeArgCount (Decl_Union u) = length (u_typeParams u)
      declTypeArgCount (Decl_Typedef t) = length (t_typeParams t)


data FieldDefaultError = FieldDefaultError Ident Ident T.Text

instance Format FieldDefaultError where
  formatText (FieldDefaultError decl field emsg) =
    T.concat ["invalid override for default of field ",field," of decl ",decl,": ", emsg]

checkDefaultOverrides :: RModule -> [FieldDefaultError]
checkDefaultOverrides m = structErrors
  where
    structErrors = concat [ structErrors1 n s | Decl{d_name=n,d_type=Decl_Struct s} <- Map.elems (m_decls m) ]
    structErrors1 n s = catMaybes [ fieldDefaultError n f | f <- s_fields s ]

    fieldDefaultError :: Ident -> Field ResolvedType -> Maybe FieldDefaultError
    fieldDefaultError n f = do
      v <- f_default f
      err <- validateLiteralForTypeExpr (f_type f) v
      return (FieldDefaultError n (f_name f) err)

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
    mapM_ checkModuleForDuplicates (Map.elems mm)
    checkModuleForDuplicates m
    let mmSorted = sortByDeps (Map.elems mm)
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

    resolveN :: [SModule] -> NameScope -> EIOT NameScope
    resolveN [] ns = return ns
    resolveN (m:ms) ns = do
        (ns',rm) <- resolve1 m ns
        resolveN ms ns'
    
    resolve1 :: SModule -> NameScope -> EIOT (NameScope,RModule)
    resolve1 m ns = do
        liftIO $ putStrLn ("processing " ++ format (m_name m) ++ "...")
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

sortByDeps :: [SModule] -> [SModule]
sortByDeps ms = map fst (sort0 (modulesWithDeps ms) Set.empty)
  where
    sort0 :: [(SModule,Set.Set ModuleName)] -> Set.Set ModuleName -> [(SModule,Set.Set ModuleName)]
    sort0 [] _ = []
    sort0 ms sofar = let (ok,todo) = partition (\(m,md)-> Set.null (md `Set.difference` sofar)) ms
                         sofar' = Set.unions (sofar:map (Set.singleton . m_name . fst) ok)
                     in ok ++ sort0 todo sofar'
                         
    modulesWithDeps ms = map (\m -> (m,getReferencedModules m)) ms

