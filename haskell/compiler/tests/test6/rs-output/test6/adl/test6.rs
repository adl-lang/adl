// @generated from adl module test6

use crate::adlrt::custom::sys::types::map::Map;
use crate::adlrt::custom::sys::types::maybe::Maybe;
use crate::adlrt::custom::sys::types::pair::Pair;
use crate::adlrt::custom::sys::types::set::Set;
use crate::test6::adl::sys::types::Either;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone,Deserialize,PartialEq,Serialize)]
pub struct S {
  pub f_pair: Pair<i32, f64>,

  pub f_either: Either<String, i32>,

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

  #[serde(default="S::def_f_int_3")]
  #[serde(rename="f_int3")]
  pub f_int_3: Option<i64>,
}

impl S {
  pub fn new(f_pair: Pair<i32, f64>, f_either: Either<String, i32>, f_map: Map<String, f64>, f_set: Set<String>, f_mstring: Maybe<String>, f_nstring: Option<String>, f_int: Option<i64>) -> S {
    S {
      f_pair: f_pair,
      f_either: f_either,
      f_map: f_map,
      f_set: f_set,
      f_mstring: f_mstring,
      f_mstring_2: S::def_f_mstring_2(),
      f_nstring: f_nstring,
      f_nstring_2: S::def_f_nstring_2(),
      f_int: f_int,
      f_int_2: S::def_f_int_2(),
      f_int_3: S::def_f_int_3(),
    }
  }

  pub fn def_f_mstring_2() -> Maybe<String> {
    Maybe::just("sukpeepolup".to_string())
  }

  pub fn def_f_nstring_2() -> Option<String> {
    Some("abcde".to_string())
  }

  pub fn def_f_int_2() -> Option<i64> {
    Some(100_i64)
  }

  pub fn def_f_int_3() -> Option<i64> {
    None
  }
}
