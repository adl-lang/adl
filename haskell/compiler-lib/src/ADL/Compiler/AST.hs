{-# LANGUAGE OverloadedStrings #-}

module ADL.Compiler.AST where

import Data.Foldable
import Data.Traversable
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Set as Set

import ADL.Utils.Format

type Ident = T.Text

type Annotations t = Map.Map ScopedName (t,JSON.Value)

newtype ModuleName = ModuleName { unModuleName :: [Ident] }
  deriving (Eq,Ord,Show)

instance Format ModuleName where
  formatText (ModuleName m) = T.intercalate "." m

data ScopedName = ScopedName {
  sn_moduleName :: ModuleName,
  sn_name :: Ident
  } deriving (Show, Eq, Ord)

instance Format ScopedName where
  formatText sn = T.intercalate "." ((unModuleName (sn_moduleName sn) ++ [sn_name sn]))
  

data Field r = Field {
  f_name :: Ident,
  f_serializedName :: Ident,
  f_type :: TypeExpr r,
  f_default :: (Maybe JSON.Value),
  f_annotations :: Annotations r
  } deriving (Show)

data Struct r = Struct {
  s_typeParams :: [Ident],
  s_fields :: [Field r]
} deriving (Show)  

data Union r = Union {
  u_typeParams :: [Ident],
  u_fields :: [Field r]
} deriving (Show)  

data Typedef r = Typedef {
  t_typeParams :: [Ident],
  t_typeExpr :: TypeExpr r
} deriving (Show)

data Newtype r = Newtype {
  n_typeParams :: [Ident],
  n_typeExpr :: TypeExpr r,
  n_default :: (Maybe JSON.Value)
} deriving (Show)

data TypeExpr r = TypeExpr r [TypeExpr r]
  deriving (Show)                  

data DeclType r = Decl_Struct (Struct r)
                | Decl_Union (Union r)
                | Decl_Typedef (Typedef r)
                | Decl_Newtype (Newtype r)
  deriving (Show)

type Version = Int
type MVersion = Maybe Version

data Decl ct r = Decl {
  d_name :: Ident,
  d_version :: MVersion,
  d_annotations :: Annotations r,
  d_type :: DeclType r,
  d_customType :: ct
  }
  deriving (Show)

data Decl0 = Decl0_Decl (Decl () ScopedName)
           | Decl0_Annotation Annotation0
  deriving (Show)

data Annotation0 = Annotation0 {
  a0_declName :: Ident,
  a0_fieldName :: Maybe Ident,
  a0_annotationName :: ScopedName,
  a0_value :: JSON.Value
  }
  deriving (Show)

data Import = Import_Module ModuleName
            | Import_ScopedName ScopedName
  deriving (Show)

iModule :: Import -> ModuleName
iModule (Import_Module m) = m           
iModule (Import_ScopedName sn) = sn_moduleName sn

-- Module after we've parsed it.
data Module0 d = Module0 {
  m0_name :: ModuleName,
  m0_imports :: [Import],
  m0_decls :: [d]
  }  
  deriving (Show)

-- Module after we've:
--    * merged the annotation declarations back into the corresponding type declarations
--    * checked for duplicate definitions
--    * checked for versioning consistency

data Module ct r = Module {
  m_name :: ModuleName,
  m_imports :: [Import],
  m_decls :: Map.Map Ident (Decl ct r)
  }  
  deriving (Show)

instance Foldable TypeExpr where
    foldMap f (TypeExpr t ts) = f t `mappend` foldMap (foldMap f) ts
instance Foldable Field where
    foldMap f (Field{f_type=t}) = foldMap f t
instance Foldable Struct where
    foldMap f Struct{s_fields=fs} = foldMap (foldMap f) fs
instance Foldable Union where
    foldMap f Union{u_fields=fs} = foldMap (foldMap f) fs
instance Foldable Typedef where
    foldMap f Typedef{t_typeExpr=t} = foldMap f t
instance Foldable Newtype where
    foldMap f Newtype{n_typeExpr=t} = foldMap f t
instance Foldable DeclType where
    foldMap f (Decl_Struct s) = foldMap f s
    foldMap f (Decl_Union u) = foldMap f u
    foldMap f (Decl_Typedef t) = foldMap f t
    foldMap f (Decl_Newtype n) = foldMap f n
instance Foldable (Decl ct) where
    foldMap f Decl{d_type=d} = foldMap f d
instance Foldable (Module ct) where
    foldMap f Module{m_decls=ds} = (foldMap.foldMap) f ds

mapAnnotations :: (a -> b) -> Annotations a -> Annotations b
mapAnnotations f = fmap (\(a,v) -> (f a,v))
  
mapTypeExpr :: (a -> b) -> TypeExpr a -> TypeExpr b
mapTypeExpr f (TypeExpr t ts) = TypeExpr (f t) (fmap (mapTypeExpr f) ts)

mapField :: (a -> b) -> Field a -> Field b
mapField f field@Field{f_type=t,f_annotations=as} = field
   { f_type=mapTypeExpr f t
   , f_annotations=mapAnnotations f as
   }

mapStruct :: (a -> b) -> Struct a -> Struct b
mapStruct f struct@Struct{s_fields=fs} = struct{s_fields=fmap (mapField f) fs}

mapUnion :: (a -> b) -> Union a -> Union b
mapUnion f union@Union{u_fields=fs} = union{u_fields=fmap (mapField f) fs}

mapTypedef :: (a -> b) -> Typedef a -> Typedef b
mapTypedef f typedef@Typedef{t_typeExpr=t} = typedef{t_typeExpr=mapTypeExpr f t}

mapNewtype :: (a -> b) -> Newtype a -> Newtype b
mapNewtype f ntype@Newtype{n_typeExpr=t} = ntype{n_typeExpr=mapTypeExpr f t}

mapDeclType :: (a -> b) -> DeclType a -> DeclType b
mapDeclType f (Decl_Struct s) = Decl_Struct (mapStruct f s)
mapDeclType f (Decl_Union u) = Decl_Union (mapUnion f u)
mapDeclType f (Decl_Typedef t) = Decl_Typedef (mapTypedef f t)
mapDeclType f (Decl_Newtype n) = Decl_Newtype (mapNewtype f n)

mapDecl :: (a -> b) -> Decl ct a -> Decl ct b
mapDecl f decl@Decl{d_type=d,d_annotations=as} = decl
  { d_type=mapDeclType f d
  , d_annotations=mapAnnotations f as
  }

mapModule :: (a -> b) -> Module ct a -> Module ct b
mapModule f mod@Module{m_decls=ds} = mod{m_decls=(fmap.mapDecl) f ds}


getReferencedModules :: Module ct ScopedName -> Set.Set ModuleName
getReferencedModules m = Set.fromList (map iModule (m_imports m)) `Set.union` foldMap ref m
  where
    ref :: ScopedName -> Set.Set ModuleName
    ref ScopedName{sn_moduleName=ModuleName []} = Set.empty
    ref sn = Set.singleton (sn_moduleName sn)
    
getTypeParams :: DeclType t -> [Ident]
getTypeParams (Decl_Struct s) = s_typeParams s
getTypeParams (Decl_Union u) = u_typeParams u
getTypeParams (Decl_Typedef t) = t_typeParams t
getTypeParams (Decl_Newtype n) = n_typeParams n
