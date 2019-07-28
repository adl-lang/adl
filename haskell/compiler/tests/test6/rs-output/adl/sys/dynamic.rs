// @generated from adl module sys.dynamic

/**
 * A serialised value along with  its type
 */
pub struct Dynamic {
  pub type_expr: crate::adl::sys::adlast::TypeExpr,
  pub value: serde_json::Value,
}

impl Dynamic {
  pub fn new(type_expr: crate::adl::sys::adlast::TypeExpr, value: serde_json::Value) -> Dynamic {
    Dynamic {
      type_expr: type_expr,
      value: value,
    }
  }
}
