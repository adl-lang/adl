// @generated from adl module sys.adlast

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
