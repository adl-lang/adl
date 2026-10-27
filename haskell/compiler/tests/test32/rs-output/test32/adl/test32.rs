// @generated from adl module test32

use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;

/**
 * An annotation carried through into the generated rust.
 */
pub type MyPath = String;

/**
 * Another, with a value that is not a string.
 */
pub type Weight = i32;

/**
 * Not marked for emission, so it must not reach the rust.
 */
pub type Ignored = String;

#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize,adl_rt::AdlAnnotations)]
#[adl_annotation(name="test32.MyPath", value="\"/decl\"")]
#[adl_annotation(name="test32.Weight", value="42")]
pub struct S {
  #[serde(rename="f1")]
  #[adl_annotation(name="test32.MyPath", value="\"/field \\\"quoted\\\" and \\\\ escaped\"")]
  pub f_1: String,

  #[serde(rename="f2")]
  pub f_2: i32,
}

impl S {
  pub fn new(f_1: String, f_2: i32) -> S {
    S {
      f_1: f_1,
      f_2: f_2,
    }
  }
}

#[derive(Clone,Deserialize,Eq,Hash,PartialEq,Serialize,adl_rt::AdlAnnotations)]
#[adl_annotation(name="test32.MyPath", value="\"/union\"")]
pub enum U {
  #[serde(rename="a")]
  #[adl_annotation(name="test32.MyPath", value="\"/branch\"")]
  A,

  #[serde(rename="b")]
  B(String),
}

#[derive(Clone,Eq,Hash,PartialEq,adl_rt::AdlAnnotations)]
#[adl_annotation(name="test32.MyPath", value="\"/newtype\"")]
pub struct N(pub String);

impl Serialize for N
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for N
{
  fn deserialize<D>(deserializer: D) -> Result<N, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = String::deserialize(deserializer)?;
      Ok(N(v))
  }
}

/**
 * A type alias has no derive attribute, and so cannot introduce the
 * attribute that an emitted annotation needs. Nothing is emitted here.
 */
pub type TD = String;
