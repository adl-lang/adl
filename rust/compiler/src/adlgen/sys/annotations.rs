// @generated from adl module sys.annotations

use serde::Deserialize;
use serde::Serialize;

pub type Doc = String;

pub type SerializedName = String;

#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct SerializedWithInternalTag {
  pub tag: String,
}

impl SerializedWithInternalTag {
  pub fn new(tag: String) -> SerializedWithInternalTag {
    SerializedWithInternalTag {
      tag: tag,
    }
  }
}

pub type CustomSerialization = bool;
