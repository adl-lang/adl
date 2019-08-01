// @generated from adl module test6

use crate::adl::sys::types::Either;
use crate::adl::sys::types::Error;
use crate::adl::sys::types::Map;
use crate::adl::sys::types::Maybe;
use crate::adl::sys::types::Pair;
use serde::ser::Serialize;
use serde::ser::SerializeStruct;
use serde::ser::Serializer;
use crate::adl::sys::types::Set;

pub struct S {
  pub f_pair: Pair<i32, f64>,
  pub f_either: Either<String, i32>,
  pub f_error: Error<i32>,
  pub f_map: Map<String, f64>,
  pub f_set: Set<String>,
  pub f_mstring: Maybe<String>,
  pub f_mstring_2: Maybe<String>,
  pub f_nstring: Option<String>,
  pub f_nstring_2: Option<String>,
  pub f_int: Option<i64>,
  pub f_int_2: Option<i64>,
}

impl S {
  pub fn new(f_pair: Pair<i32, f64>, f_either: Either<String, i32>, f_error: Error<i32>, f_map: Map<String, f64>, f_set: Set<String>, f_mstring: Maybe<String>, f_nstring: Option<String>, f_int: Option<i64>) -> S {
    S {
      f_pair: f_pair,
      f_either: f_either,
      f_error: f_error,
      f_map: f_map,
      f_set: f_set,
      f_mstring: f_mstring,
      f_mstring_2: Maybe::Just("sukpeepolup".to_string()),
      f_nstring: f_nstring,
      f_nstring_2: Some("abcde".to_string()),
      f_int: f_int,
      f_int_2: Some(100_i64),
    }
  }
}

impl Serialize for S {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("S", 11)?;
    s.serialize_field("f_pair", &self.f_pair)?;
    s.serialize_field("f_either", &self.f_either)?;
    s.serialize_field("f_error", &self.f_error)?;
    s.serialize_field("f_map", &self.f_map)?;
    s.serialize_field("f_set", &self.f_set)?;
    s.serialize_field("f_mstring", &self.f_mstring)?;
    s.serialize_field("f_mstring2", &self.f_mstring_2)?;
    s.serialize_field("f_nstring", &self.f_nstring)?;
    s.serialize_field("f_nstring2", &self.f_nstring_2)?;
    s.serialize_field("f_int", &self.f_int)?;
    s.serialize_field("f_int2", &self.f_int_2)?;
    s.end()
  }
}
