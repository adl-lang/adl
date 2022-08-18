use crate::adlgen::sys::{adlast2 as adlast};

pub mod annotations;
pub mod resolver;
pub mod primitives;

pub type TypeExpr0 = adlast::TypeExpr<adlast::ScopedName>;
pub type Module0 = adlast::Module<TypeExpr0>;

pub trait AdlLoader {
  /// Find and load the specified ADL module
  fn load(module_name: adlast::ModuleName) -> Result<Module0, anyhow::Error>;
}

