use crate::adlgen::sys::{adlast2 as adlast};

type TypeExpr0 = adlast::TypeExpr<adlast::ScopedName>;
pub type Module0 = adlast::Module<TypeExpr0>;

pub mod annotations;

pub trait AdlLoader {
  /// Find and load the specified ADL module
  fn load(module_name: adlast::ModuleName) -> Result<Module0, anyhow::Error>;
}

