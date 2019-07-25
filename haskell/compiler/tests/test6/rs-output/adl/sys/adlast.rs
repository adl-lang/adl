// @generated from adl module sys.adlast

pub type ModuleName = String;

pub type Ident = String;

pub type Annotations = crate::adl::sys::types::Map<ScopedName, serde_json::Value>;

pub struct ScopedName {
  pub module_name: ModuleName,
  pub name: Ident,
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

pub struct Field {
  pub name: Ident,
  pub serialized_name: Ident,
  pub type_expr: TypeExpr,
  pub default: crate::adl::sys::types::Maybe<serde_json::Value>,
  pub annotations: Annotations,
}

pub struct Struct {
  pub type_params: Vec<Ident>,
  pub fields: Vec<Field>,
}

pub struct Union {
  pub type_params: Vec<Ident>,
  pub fields: Vec<Field>,
}

pub struct TypeDef {
  pub type_params: Vec<Ident>,
  pub type_expr: TypeExpr,
}

pub struct NewType {
  pub type_params: Vec<Ident>,
  pub type_expr: TypeExpr,
  pub default: crate::adl::sys::types::Maybe<serde_json::Value>,
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

pub struct ScopedDecl {
  pub module_name: ModuleName,
  pub decl: Decl,
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
