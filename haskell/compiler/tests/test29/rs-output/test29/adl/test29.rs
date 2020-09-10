// @generated from adl module test29

use serde::Deserialize;
use serde::Serialize;

#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Test {
  #[serde(default="Test::def_foo")]
  pub foo: std::collections::HashMap<String,String>,
}

impl Test {
  pub fn new() -> Test {
    Test {
      foo: Test::def_foo(),
    }
  }

  pub fn def_foo() -> std::collections::HashMap<String,String> {
    [(" ".to_string(), "baz".to_string()), ("\"".to_string(), "baz".to_string()), ("$".to_string(), "bar".to_string()), ("'".to_string(), "baz".to_string())].iter().cloned().collect()
  }
}
