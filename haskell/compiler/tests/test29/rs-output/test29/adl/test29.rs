// @generated from adl module test29

use serde::Deserialize;
use serde::Serialize;

/**
 * An example with weird "quoting" conventions, designed to break things
 */
#[derive(Clone,Debug,Deserialize,Eq,PartialEq,Serialize)]
pub struct Test {
  /**
   * "foo" as a field
   */
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
    [(" ".to_string(), "baz".to_string()), ("\"".to_string(), "baz".to_string()), ("$".to_string(), "bar".to_string()), ("'".to_string(), "baz".to_string()), ("degrees".to_string(), "°".to_string())].iter().cloned().collect()
  }
}
