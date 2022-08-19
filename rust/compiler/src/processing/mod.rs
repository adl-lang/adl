use crate::adlgen::sys::adlast2 as adlast;

pub mod annotations;
pub mod loader;
pub mod primitives;
pub mod resolver;

pub type TypeExpr0 = adlast::TypeExpr<adlast::ScopedName>;
pub type Module0 = adlast::Module<TypeExpr0>;
