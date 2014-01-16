{-# LANGUAGE OverloadedStrings #-}

module ADL.Compiler.AST where

import Data.Foldable
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Set as Set

import ADL.Utils.Format

type Annotations = Map.Map String String

type Ident = T.Text

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
  

data Field t = Field {
  f_name :: Ident,
  f_type :: TypeExpr t,
  f_default :: (Maybe JSON.Value),
  f_annotations :: Annotations
  } deriving (Show)

data Struct t = Struct {
  s_typeParams :: [Ident],
  s_fields :: [Field t]
} deriving (Show)  

data Union t = Union {
  u_typeParams :: [Ident],
  u_fields :: [Field t]
} deriving (Show)  

data Typedef t = Typedef {
  t_typeParams :: [Ident],
  t_typeExpr :: TypeExpr t
} deriving (Show)

data Newtype t = Newtype {
  n_typeParams :: [Ident],
  n_typeExpr :: TypeExpr t,
  n_default :: (Maybe JSON.Value)
} deriving (Show)

data TypeExpr t = TypeExpr t [TypeExpr t]
  deriving (Show)                  

data DeclType t = Decl_Struct (Struct t)
                | Decl_Union (Union t)
                | Decl_Typedef (Typedef t)
                | Decl_Newtype (Newtype t)
  deriving (Show)

type Version = Int
type MVersion = Maybe Version

data Decl t = Decl {
  d_name :: Ident,
  d_version :: MVersion,
  d_annotations :: Annotations,
  d_type :: DeclType t
  }
  deriving (Show)

data Import = Import_Module ModuleName
            | Import_ScopedName ScopedName
  deriving (Show)

iModule :: Import -> ModuleName
iModule (Import_Module m) = m           
iModule (Import_ScopedName sn) = sn_moduleName sn

-- Module after we've parsed it.
data Module0 t = Module0 {
  m0_name :: ModuleName,
  m0_imports :: [Import],
  m0_decls :: [Decl t]
  }  
  deriving (Show)

-- Module after we've checked for duplicate definitions
-- and for versioning consistency
data Module t = Module {
  m_name :: ModuleName,
  m_imports :: [Import],
  m_decls :: Map.Map Ident (Decl t)
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
instance Foldable Decl where
    foldMap f Decl{d_type=d} = foldMap f d
instance Foldable Module where
    foldMap f Module{m_decls=ds} = (foldMap.foldMap) f ds

getReferencedModules :: Module ScopedName -> Set.Set ModuleName
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