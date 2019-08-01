// @generated from adl module sys.adlast

use serde::ser::Serialize;
use serde::ser::SerializeStruct;
use serde::ser::Serializer;

pub type ModuleName = String;

pub type Ident = String;

pub type Annotations = crate::adl::sys::types::Map<ScopedName, serde_json::Value>;

pub struct ScopedName {
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

impl Serialize for ScopedName {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("ScopedName", 2)?;
    s.serialize_field("moduleName", &self.module_name)?;
    s.serialize_field("name", &self.name)?;
    s.end()
  }
}

pub enum TypeRef {
  Primitive(Ident),
  TypeParam(Ident),
  Reference(ScopedName),
}

pub struct TypeExpr {
  pub type_ref: TypeRef,
  pub parameters: Vec<TypeExpr>,
}

impl TypeExpr {
  pub fn new(type_ref: TypeRef, parameters: Vec<TypeExpr>) -> TypeExpr {
    TypeExpr {
      type_ref: type_ref,
      parameters: parameters,
    }
  }
}

impl Serialize for TypeExpr {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("TypeExpr", 2)?;
    s.serialize_field("typeRef", &self.type_ref)?;
    s.serialize_field("parameters", &self.parameters)?;
    s.end()
  }
}

pub struct Field {
  pub name: Ident,
  pub serialized_name: Ident,
  pub type_expr: TypeExpr,
  pub default: crate::adl::sys::types::Maybe<serde_json::Value>,
  pub annotations: Annotations,
}

impl Field {
  pub fn new(name: Ident, serialized_name: Ident, type_expr: TypeExpr, default: crate::adl::sys::types::Maybe<serde_json::Value>, annotations: Annotations) -> Field {
    Field {
      name: name,
      serialized_name: serialized_name,
      type_expr: type_expr,
      default: default,
      annotations: annotations,
    }
  }
}

impl Serialize for Field {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Field", 5)?;
    s.serialize_field("name", &self.name)?;
    s.serialize_field("serializedName", &self.serialized_name)?;
    s.serialize_field("typeExpr", &self.type_expr)?;
    s.serialize_field("default", &self.default)?;
    s.serialize_field("annotations", &self.annotations)?;
    s.end()
  }
}

pub struct Struct {
  pub type_params: Vec<Ident>,
  pub fields: Vec<Field>,
}

impl Struct {
  pub fn new(type_params: Vec<Ident>, fields: Vec<Field>) -> Struct {
    Struct {
      type_params: type_params,
      fields: fields,
    }
  }
}

impl Serialize for Struct {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Struct", 2)?;
    s.serialize_field("typeParams", &self.type_params)?;
    s.serialize_field("fields", &self.fields)?;
    s.end()
  }
}

pub struct Union {
  pub type_params: Vec<Ident>,
  pub fields: Vec<Field>,
}

impl Union {
  pub fn new(type_params: Vec<Ident>, fields: Vec<Field>) -> Union {
    Union {
      type_params: type_params,
      fields: fields,
    }
  }
}

impl Serialize for Union {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Union", 2)?;
    s.serialize_field("typeParams", &self.type_params)?;
    s.serialize_field("fields", &self.fields)?;
    s.end()
  }
}

pub struct TypeDef {
  pub type_params: Vec<Ident>,
  pub type_expr: TypeExpr,
}

impl TypeDef {
  pub fn new(type_params: Vec<Ident>, type_expr: TypeExpr) -> TypeDef {
    TypeDef {
      type_params: type_params,
      type_expr: type_expr,
    }
  }
}

impl Serialize for TypeDef {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("TypeDef", 2)?;
    s.serialize_field("typeParams", &self.type_params)?;
    s.serialize_field("typeExpr", &self.type_expr)?;
    s.end()
  }
}

pub struct NewType {
  pub type_params: Vec<Ident>,
  pub type_expr: TypeExpr,
  pub default: crate::adl::sys::types::Maybe<serde_json::Value>,
}

impl NewType {
  pub fn new(type_params: Vec<Ident>, type_expr: TypeExpr, default: crate::adl::sys::types::Maybe<serde_json::Value>) -> NewType {
    NewType {
      type_params: type_params,
      type_expr: type_expr,
      default: default,
    }
  }
}

impl Serialize for NewType {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("NewType", 3)?;
    s.serialize_field("typeParams", &self.type_params)?;
    s.serialize_field("typeExpr", &self.type_expr)?;
    s.serialize_field("default", &self.default)?;
    s.end()
  }
}

pub enum DeclType {
  Struct(Struct),
  Union(Union),
  Type(TypeDef),
  Newtype(NewType),
}

pub struct Decl {
  pub name: Ident,
  pub version: crate::adl::sys::types::Maybe<u32>,
  pub r#type: DeclType,
  pub annotations: Annotations,
}

impl Decl {
  pub fn new(name: Ident, version: crate::adl::sys::types::Maybe<u32>, r#type: DeclType, annotations: Annotations) -> Decl {
    Decl {
      name: name,
      version: version,
      r#type: r#type,
      annotations: annotations,
    }
  }
}

impl Serialize for Decl {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Decl", 4)?;
    s.serialize_field("name", &self.name)?;
    s.serialize_field("version", &self.version)?;
    s.serialize_field("type_", &self.r#type)?;
    s.serialize_field("annotations", &self.annotations)?;
    s.end()
  }
}

pub struct ScopedDecl {
  pub module_name: ModuleName,
  pub decl: Decl,
}

impl ScopedDecl {
  pub fn new(module_name: ModuleName, decl: Decl) -> ScopedDecl {
    ScopedDecl {
      module_name: module_name,
      decl: decl,
    }
  }
}

impl Serialize for ScopedDecl {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("ScopedDecl", 2)?;
    s.serialize_field("moduleName", &self.module_name)?;
    s.serialize_field("decl", &self.decl)?;
    s.end()
  }
}

pub type DeclVersions = Vec<Decl>;

pub enum Import {
  ModuleName(ModuleName),
  ScopedName(ScopedName),
}

pub struct Module {
  pub name: ModuleName,
  pub imports: Vec<Import>,
  pub decls: std::collections::HashMap<String,Decl>,
  pub annotations: Annotations,
}

impl Module {
  pub fn new(name: ModuleName, imports: Vec<Import>, decls: std::collections::HashMap<String,Decl>, annotations: Annotations) -> Module {
    Module {
      name: name,
      imports: imports,
      decls: decls,
      annotations: annotations,
    }
  }
}

impl Serialize for Module {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Module", 4)?;
    s.serialize_field("name", &self.name)?;
    s.serialize_field("imports", &self.imports)?;
    s.serialize_field("decls", &self.decls)?;
    s.serialize_field("annotations", &self.annotations)?;
    s.end()
  }
}
