// @generated from adl module sys.dynamic

use serde::Deserialize;
use serde::Serialize;
use crate::adl::sys::adlast::TypeExpr;

/**
 * A serialised value along with  its type
 */
#[derive(Serialize,Deserialize)]
pub struct Dynamic {
  #[serde(rename="typeExpr")]
  pub type_expr: TypeExpr,

  pub value: serde_json::Value,
}

impl Dynamic {
  pub fn new(type_expr: TypeExpr, value: serde_json::Value) -> Dynamic {
    Dynamic {
      type_expr: type_expr,
      value: value,
    }
  }
}
