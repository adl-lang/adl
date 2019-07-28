// @generated from adl module test6

pub struct S {
  pub f_pair: crate::adl::sys::types::Pair<i32, f64>,
  pub f_either: crate::adl::sys::types::Either<String, i32>,
  pub f_error: crate::adl::sys::types::Error<i32>,
  pub f_map: crate::adl::sys::types::Map<String, f64>,
  pub f_set: crate::adl::sys::types::Set<String>,
  pub f_mstring: crate::adl::sys::types::Maybe<String>,
  pub f_mstring_2: crate::adl::sys::types::Maybe<String>,
  pub f_nstring: Option<String>,
  pub f_nstring_2: Option<String>,
  pub f_int: Option<i64>,
  pub f_int_2: Option<i64>,
}

impl S {
  pub fn new(f_pair: crate::adl::sys::types::Pair<i32, f64>, f_either: crate::adl::sys::types::Either<String, i32>, f_error: crate::adl::sys::types::Error<i32>, f_map: crate::adl::sys::types::Map<String, f64>, f_set: crate::adl::sys::types::Set<String>, f_mstring: crate::adl::sys::types::Maybe<String>, f_nstring: Option<String>, f_int: Option<i64>) -> S {
    S {
      f_pair: f_pair,
      f_either: f_either,
      f_error: f_error,
      f_map: f_map,
      f_set: f_set,
      f_mstring: f_mstring,
      f_mstring_2: crate::adl::sys::types::Maybe::Just("sukpeepolup".to_string()),
      f_nstring: f_nstring,
      f_nstring_2: Some("abcde".to_string()),
      f_int: f_int,
      f_int_2: Some(100_i64),
    }
  }
}
