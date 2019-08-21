// @generated from adl module test6

use crate::test6::adl::sys::types::Either;
use crate::test6::adl::sys::types::Error;
use crate::test6::adl::sys::types::Map;
use crate::test6::adl::sys::types::Maybe;
use crate::test6::adl::sys::types::Pair;
use crate::test6::adl::sys::types::Set;
use serde::Deserialize;
use serde::Serialize;

#[derive(Deserialize,PartialEq,Serialize)]
pub struct S {
  pub f_pair: Pair<i32, f64>,

  pub f_either: Either<String, i32>,

  pub f_error: Error<i32>,

  pub f_map: Map<String, f64>,

  pub f_set: Set<String>,

  pub f_mstring: Maybe<String>,

  #[serde(default="S::def_f_mstring_2")]
  #[serde(rename="f_mstring2")]
  pub f_mstring_2: Maybe<String>,

  pub f_nstring: Option<String>,

  #[serde(default="S::def_f_nstring_2")]
  #[serde(rename="f_nstring2")]
  pub f_nstring_2: Option<String>,

  pub f_int: Option<i64>,

  #[serde(default="S::def_f_int_2")]
  #[serde(rename="f_int2")]
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
      f_mstring_2: S::def_f_mstring_2(),
      f_nstring: f_nstring,
      f_nstring_2: S::def_f_nstring_2(),
      f_int: f_int,
      f_int_2: S::def_f_int_2(),
    }
  }

  pub fn def_f_mstring_2() -> Maybe<String> {
    Maybe::Just("sukpeepolup".to_string())
  }

  pub fn def_f_nstring_2() -> Option<String> {
    Some("abcde".to_string())
  }

  pub fn def_f_int_2() -> Option<i64> {
    Some(100_i64)
  }
}
