// @generated from adl module sys.adlast2

use crate::adlrt::custom::sys::types::map::Map;
use crate::adlrt::custom::sys::types::maybe::Maybe;
use serde::Deserialize;
use serde::Serialize;

pub type ModuleName = String;

pub type Ident = String;

pub type Annotations = Map<ScopedName, serde_json::Value>;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct ScopedName {
  #[serde(rename="moduleName")]
  pub module_name: ModuleName,

  pub name: Ident,
}

impl ScopedName {
  pub fn new(module_name: ModuleName, name: Ident) -> ScopedName {
    ScopedName {
      module_name: module_name,
      name: name,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct TypeExpr<R> {
  #[serde(rename="typeRef")]
  pub type_ref: R,

  pub parameters: Vec<TypeExpr<R>>,
}

impl<R> TypeExpr<R> {
  pub fn new(type_ref: R, parameters: Vec<TypeExpr<R>>) -> TypeExpr<R> {
    TypeExpr {
      type_ref: type_ref,
      parameters: parameters,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct Field<TE> {
  pub name: Ident,

  #[serde(rename="serializedName")]
  pub serialized_name: Ident,

  #[serde(rename="typeExpr")]
  pub type_expr: TE,

  pub default: Maybe<serde_json::Value>,

  pub annotations: Annotations,
}

impl<TE> Field<TE> {
  pub fn new(name: Ident, serialized_name: Ident, type_expr: TE, default: Maybe<serde_json::Value>, annotations: Annotations) -> Field<TE> {
    Field {
      name: name,
      serialized_name: serialized_name,
      type_expr: type_expr,
      default: default,
      annotations: annotations,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct Struct<TE> {
  #[serde(rename="typeParams")]
  pub type_params: Vec<Ident>,

  pub fields: Vec<Field<TE>>,
}

impl<TE> Struct<TE> {
  pub fn new(type_params: Vec<Ident>, fields: Vec<Field<TE>>) -> Struct<TE> {
    Struct {
      type_params: type_params,
      fields: fields,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct Union<TE> {
  #[serde(rename="typeParams")]
  pub type_params: Vec<Ident>,

  pub fields: Vec<Field<TE>>,
}

impl<TE> Union<TE> {
  pub fn new(type_params: Vec<Ident>, fields: Vec<Field<TE>>) -> Union<TE> {
    Union {
      type_params: type_params,
      fields: fields,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct TypeDef<TE> {
  #[serde(rename="typeParams")]
  pub type_params: Vec<Ident>,

  #[serde(rename="typeExpr")]
  pub type_expr: TE,
}

impl<TE> TypeDef<TE> {
  pub fn new(type_params: Vec<Ident>, type_expr: TE) -> TypeDef<TE> {
    TypeDef {
      type_params: type_params,
      type_expr: type_expr,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct NewType<TE> {
  #[serde(rename="typeParams")]
  pub type_params: Vec<Ident>,

  #[serde(rename="typeExpr")]
  pub type_expr: TE,

  pub default: Maybe<serde_json::Value>,
}

impl<TE> NewType<TE> {
  pub fn new(type_params: Vec<Ident>, type_expr: TE, default: Maybe<serde_json::Value>) -> NewType<TE> {
    NewType {
      type_params: type_params,
      type_expr: type_expr,
      default: default,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub enum DeclType<TE> {
  #[serde(rename="struct_")]
  Struct(Struct<TE>),

  #[serde(rename="union_")]
  Union(Union<TE>),

  #[serde(rename="type_")]
  Type(TypeDef<TE>),

  #[serde(rename="newtype_")]
  Newtype(NewType<TE>),
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct Decl<TE> {
  pub name: Ident,

  pub version: Maybe<u32>,

  #[serde(rename="type_")]
  pub r#type: DeclType<TE>,

  pub annotations: Annotations,
}

impl<TE> Decl<TE> {
  pub fn new(name: Ident, version: Maybe<u32>, r#type: DeclType<TE>, annotations: Annotations) -> Decl<TE> {
    Decl {
      name: name,
      version: version,
      r#type: r#type,
      annotations: annotations,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct ScopedDecl<TE> {
  #[serde(rename="moduleName")]
  pub module_name: ModuleName,

  pub decl: Decl<TE>,
}

impl<TE> ScopedDecl<TE> {
  pub fn new(module_name: ModuleName, decl: Decl<TE>) -> ScopedDecl<TE> {
    ScopedDecl {
      module_name: module_name,
      decl: decl,
    }
  }
}

pub type DeclVersions<TE> = Vec<Decl<TE>>;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum Import {
  #[serde(rename="moduleName")]
  ModuleName(ModuleName),

  #[serde(rename="scopedName")]
  ScopedName(ScopedName),
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct Module<TE> {
  pub name: ModuleName,

  pub imports: Vec<Import>,

  pub decls: Vec<Decl<TE>>,

  pub annotations: Annotations,
}

impl<TE> Module<TE> {
  pub fn new(name: ModuleName, imports: Vec<Import>, decls: Vec<Decl<TE>>, annotations: Annotations) -> Module<TE> {
    Module {
      name: name,
      imports: imports,
      decls: decls,
      annotations: annotations,
    }
  }
}

/**
 * The Span start..end contains all values with start <= x < end. It is empty if start >= end.
 */
#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Span {
  pub start: u64,

  pub end: u64,
}

impl Span {
  pub fn new(start: u64, end: u64) -> Span {
    Span {
      start: start,
      end: end,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Spanned<T> {
  pub value: T,

  pub span: Span,
}

impl<T> Spanned<T> {
  pub fn new(value: T, span: Span) -> Spanned<T> {
    Spanned {
      value: value,
      span: span,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum PrimitiveType {
  Void,

  Bool,

  Int8,

  Int16,

  Int32,

  Int64,

  Word8,

  Word16,

  Word32,

  Word64,

  Float,

  Double,

  Json,

  ByteVector,

  String,

  Vector,

  StringMap,

  Nullable,

  TypeToken,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum TypeRef {
  #[serde(rename="scopedName")]
  ScopedName(ScopedName),

  #[serde(rename="localName")]
  LocalName(Ident),

  #[serde(rename="primitive")]
  Primitive(PrimitiveType),

  #[serde(rename="typeParam")]
  TypeParam(Ident),
}

pub type TypeExpr0 = TypeExpr<ScopedName>;

pub type Field0 = Field<TypeExpr0>;

pub type Struct0 = Struct<TypeExpr0>;

pub type Union0 = Union<TypeExpr0>;

pub type DeclType0 = DeclType<TypeExpr0>;

pub type Decl0 = Decl<TypeExpr0>;

pub type Module0 = Module<TypeExpr0>;

pub type TypeExpr1 = TypeExpr<TypeRef>;

pub type Field1 = Field<TypeExpr1>;

pub type Struct1 = Struct<TypeExpr1>;

pub type Union1 = Union<TypeExpr1>;

pub type DeclType1 = DeclType<TypeExpr1>;

pub type Decl1 = Decl<TypeExpr1>;

pub type Module1 = Module<TypeExpr1>;

pub type AdlAst = std::collections::HashMap<String,AstModule>;

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct AstModule {
  pub name: ModuleName,

  pub imports: Vec<Import>,

  pub decls: std::collections::HashMap<String,Decl<TypeExpr1>>,

  pub annotations: Annotations,
}

impl AstModule {
  pub fn new(name: ModuleName, imports: Vec<Import>, decls: std::collections::HashMap<String,Decl<TypeExpr1>>, annotations: Annotations) -> AstModule {
    AstModule {
      name: name,
      imports: imports,
      decls: decls,
      annotations: annotations,
    }
  }
}
