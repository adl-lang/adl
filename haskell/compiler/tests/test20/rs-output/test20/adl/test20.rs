// @generated from adl module test20

use serde::Deserialize;
use serde::Serialize;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum Role {
  #[serde(rename="u")]
  Underling,

  #[serde(rename="b")]
  Boss,

  #[serde(rename="sb")]
  SuperBoss,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Person {
  #[serde(rename="fn")]
  pub first_name: String,

  #[serde(rename="ln")]
  pub last_name: String,

  pub age: i16,

  pub role: Role,
}

impl Person {
  pub fn new(first_name: String, last_name: String, age: i16, role: Role) -> Person {
    Person {
      first_name: first_name,
      last_name: last_name,
      age: age,
      role: role,
    }
  }
}
