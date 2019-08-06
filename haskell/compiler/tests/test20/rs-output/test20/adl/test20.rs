// @generated from adl module test20

use serde::Deserialize;
use serde::Serialize;

#[derive(Serialize,Deserialize)]
pub enum Role {
  #[serde(rename="underling")]
  Underling,

  #[serde(rename="boss")]
  Boss,

  #[serde(rename="superBoss")]
  SuperBoss,
}

#[derive(Serialize,Deserialize)]
pub struct Person {
  #[serde(rename="firstName")]
  pub first_name: String,

  #[serde(rename="lastName")]
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
