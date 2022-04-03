// @generated from adl module adlc.config.rust

use serde::Deserialize;
use serde::Serialize;

/**
 * ADL module or declaration annotation to control
 * whether code is actually generated.
 */
pub type RustGenerate = bool;

/**
 * ADL field annotation giving explicit control over
 * how a field should be stored.
 */
#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum RustStorageModel {
  /**
   * Standard inline storage
   */
  #[serde(rename="standard")]
  Standard,

  /**
   * Store the value in a Box<>
   */
  #[serde(rename="boxed")]
  Boxed,
}

/**
 * ADL declaration annotation to specify that a custom type
 * should be used
 */
#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct RustCustomType {
  pub rustname: String,

  pub helpers: String,

  #[serde(default="RustCustomType::def_generate_orig_adl_type")]
  #[serde(rename="generateOrigADLType")]
  pub generate_orig_adl_type: String,

  #[serde(rename="stdTraits")]
  pub std_traits: Vec<String>,
}

impl RustCustomType {
  pub fn new(rustname: String, helpers: String, std_traits: Vec<String>) -> RustCustomType {
    RustCustomType {
      rustname: rustname,
      helpers: helpers,
      generate_orig_adl_type: RustCustomType::def_generate_orig_adl_type(),
      std_traits: std_traits,
    }
  }

  pub fn def_generate_orig_adl_type() -> String {
    "".to_string()
  }
}
