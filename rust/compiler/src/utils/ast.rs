
use crate::adlgen::sys::{adlast2 as adlast};

pub fn mk_scoped_name(mname: &str, name: &str) -> adlast::ScopedName {
  adlast::ScopedName::new(mname.to_string(), name.to_string())
}

pub fn mk_typeexpr0(type_ref: adlast::ScopedName) -> adlast::TypeExpr<adlast::ScopedName> {
  adlast::TypeExpr{type_ref, parameters: vec![]}
}
