// @generated from adl module test14

use serde::Deserialize;
use serde::Serialize;

#[derive(Clone,Deserialize,PartialEq,Serialize)]
pub struct Switch {
  pub double: f64,

  pub int: i32,

  pub string: String,

  #[serde(rename="for")]
  pub r#for: bool,

  #[serde(rename="Objects")]
  pub objects: String,
}

impl Switch {
  pub fn new(double: f64, int: i32, string: String, r#for: bool, objects: String) -> Switch {
    Switch {
      double: double,
      int: int,
      string: string,
      r#for: r#for,
      objects: objects,
    }
  }
}

#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum Unsigned {
  #[serde(rename="null")]
  Null,
}

#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Factory(pub String);
