{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Processing where

import Debug.Trace

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.List(intercalate,find)
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

import qualified ParserP as P

import ADL.Utils.Format

import AST
import Primitive
import EIO

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
      checkTypeExpr ns (TE_Ref sn) = checkScopedName ns sn
      checkTypeExpr ns (TE_Apply t args) = checkScopedName ns t `mappend` foldMap (checkTypeExpr ns) args

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
    resolveTypeExpr ns (TE_Ref sn) = TE_Ref (resolveName ns sn)
    resolveTypeExpr ns (TE_Apply t args) = TE_Apply (resolveName ns t) (map (resolveTypeExpr ns) args)

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
      checkTypeExpr (TE_Apply t exprs) = checkTypeCtorApp t exprs `mappend` foldMap checkTypeExpr exprs
      checkTypeExpr (TE_Ref t) = checkTypeCtorApp t []

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
validateLiteralForTypeExpr te v = validateTE te v
  where
    validateTE (TE_Ref (RT_Named (sn,decl))) v = case d_type decl of
      (Decl_Struct s) -> structLiteral s v
      (Decl_Union u) -> unionLiteral u v 
      (Decl_Typedef t) -> validateTE (t_typeExpr t) v
    validateTE (TE_Ref (RT_Param id)) v = Just "literals for parameterised types not yet supported"
    validateTE (TE_Ref (RT_Primitive pt)) v = ptValidateLiteral pt v
    validateTE (TE_Apply (RT_Primitive P_Vector) rts) v = case rts of
      [rt] -> vecLiteral rt v
      _ -> error "INTERNAL ERROR: found vector with 0 or >2 type parameters post type checking"
    validateTE (TE_Apply rt rts) v = Just "literals for custom parameterised types not yet supported"
    
    vecLiteral rt (JSON.Array v) = case catMaybes errs of
      [] -> Nothing
      (e:_) -> (Just e)
      where
        errs = map (validateTE rt) (V.toList v)
    vecLiteral rt _ = Just "expected an array"

    structLiteral s (JSON.Object hm) = Just "literals for struct values not yet supported"
    structLiteral s _ = Just "expected an object"

    unionLiteral u (JSON.Object hm) = case HM.toList hm of
      [(s,v)] -> case find ((s==).f_name) (u_fields u) of
        (Just f) -> validateTE (f_type f) v
        Nothing -> Just "literal union doesn't match any field name"
      _ -> Just "literal union must have a single key/value pair"
    unionLiteral s _ = Just "expected an object"

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

