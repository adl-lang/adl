// @generated from adl module test14

use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
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

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum Unsigned {
  #[serde(rename="null")]
  Null,
}

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Factory(pub String);

impl Serialize for Factory
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for Factory
{
  fn deserialize<D>(deserializer: D) -> Result<Factory, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = String::deserialize(deserializer)?;
      Ok(Factory(v))
  }
}
