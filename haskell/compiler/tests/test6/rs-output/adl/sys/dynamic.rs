// @generated from adl module sys.dynamic

use serde::ser::Serialize;
use serde::ser::SerializeStruct;
use serde::ser::Serializer;

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

impl Serialize for Dynamic {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Dynamic", 2)?;
    s.serialize_field("typeExpr", &self.type_expr)?;
    s.serialize_field("value", &self.value)?;
    s.end()
  }
}
