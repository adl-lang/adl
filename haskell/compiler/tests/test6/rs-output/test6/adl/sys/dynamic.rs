// @generated from adl module sys.dynamic

use crate::test6::adl::sys::adlast::TypeExpr;
use serde::Deserialize;
use serde::Serialize;

/**
 * A serialised value along with  its type
 */
#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
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
