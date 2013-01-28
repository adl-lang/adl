module AST where

import qualified Data.Text as T

import qualified Data.Map as Map

data JSONValue = JSONValue
  deriving (Show)
           
type Literal = JSONValue

type Annotations = Map.Map String String

type Ident = T.Text

type ModuleName = [Ident]

data ScopedName = ScopedName {
  sn_moduleName :: ModuleName,
  sn_name :: Ident
  } deriving (Show)

data Field t = Field {
  f_name :: Ident,
  f_type :: TypeExpr t,
  f_default :: (Maybe Literal),
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

data TypeExpr t = TE_Ref t
                | TE_Apply t [TypeExpr t]
  deriving (Show)                  

data DeclType t = Decl_Struct (Struct t)
                | Decl_Union (Union t)
                | Decl_Typedef (Typedef t)
  deriving (Show)

data Decl t = Decl {
  d_name :: Ident,
  d_annotations :: Annotations,
  d_type :: DeclType t
  }
  deriving (Show)

data Module t = Module {
  m_name :: ModuleName,
  m_imports :: [ModuleName],
  m_decls :: [Decl t]
  }  
  deriving (Show)

newtype NameRefDecl = NameRefDecl (Decl ScopedName)
newtype ResolvedDecl = ResolvedDecl (Decl ResolvedDecl)

resolveTypes :: Map.Map ScopedName NameRefDecl
             -> Map.Map ScopedName ResolvedDecl
resolveTypes map = rmap
  where
    rmap = Map.map resolve1 map

    resolve1 :: NameRefDecl -> ResolvedDecl
    resolve1 (NameRefDecl d) = ResolvedDecl d{d_type=resolve2 (d_type d)}

    resolve2 :: DeclType ScopedName -> DeclType ResolvedDecl
    resolve2 = undefined

