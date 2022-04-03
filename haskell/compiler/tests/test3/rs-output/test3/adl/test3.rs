// @generated from adl module test3

use base64;
use serde::Deserialize;
use serde::Serialize;
use serde_json;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct A {
  pub f_int: i16,

  pub f_string: String,

  #[serde(default="A::def_f_bool")]
  pub f_bool: bool,
}

impl A {
  pub fn new(f_int: i16, f_string: String) -> A {
    A {
      f_int: f_int,
      f_string: f_string,
      f_bool: A::def_f_bool(),
    }
  }

  pub fn def_f_bool() -> bool {
    false
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct XY<T> {
  pub x: T,

  pub y: T,
}

impl<T> XY<T> {
  pub fn new(x: T, y: T) -> XY<T> {
    XY {
      x: x,
      y: y,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct B<T> {
  pub f_t: T,

  pub f_string: String,

  pub f_tvec: Vec<T>,

  pub f_xy: XY<T>,
}

impl<T> B<T> {
  pub fn new(f_t: T, f_string: String, f_tvec: Vec<T>, f_xy: XY<T>) -> B<T> {
    B {
      f_t: f_t,
      f_string: f_string,
      f_tvec: f_tvec,
      f_xy: f_xy,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U {
  #[serde(rename="f_int")]
  FInt(i16),

  #[serde(rename="f_string")]
  FString(String),

  #[serde(rename="f_void")]
  FVoid,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum E {
  #[serde(rename="v1")]
  V1,

  #[serde(rename="v2")]
  V2,
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct S<T> {
  #[serde(default="S::<T>::def_f_void")]
  pub f_void: (),

  #[serde(default="S::<T>::def_f_bool")]
  pub f_bool: bool,

  #[serde(default="S::<T>::def_f_int_8")]
  #[serde(rename="f_int8")]
  pub f_int_8: i8,

  #[serde(default="S::<T>::def_f_int_16")]
  #[serde(rename="f_int16")]
  pub f_int_16: i16,

  #[serde(default="S::<T>::def_f_int_32")]
  #[serde(rename="f_int32")]
  pub f_int_32: i32,

  #[serde(default="S::<T>::def_f_int_64")]
  #[serde(rename="f_int64")]
  pub f_int_64: i64,

  #[serde(default="S::<T>::def_f_word_8")]
  #[serde(rename="f_word8")]
  pub f_word_8: u8,

  #[serde(default="S::<T>::def_f_word_16")]
  #[serde(rename="f_word16")]
  pub f_word_16: u16,

  #[serde(default="S::<T>::def_f_word_32")]
  #[serde(rename="f_word32")]
  pub f_word_32: u32,

  #[serde(default="S::<T>::def_f_word_64")]
  #[serde(rename="f_word64")]
  pub f_word_64: u64,

  #[serde(default="S::<T>::def_f_float")]
  pub f_float: f32,

  #[serde(default="S::<T>::def_f_double")]
  pub f_double: f64,

  #[serde(default="S::<T>::def_f_bytes")]
  pub f_bytes: Vec<u8>,

  #[serde(default="S::<T>::def_f_string")]
  pub f_string: String,

  #[serde(default="S::<T>::def_f_vstring")]
  pub f_vstring: Vec<String>,

  #[serde(default="S::<T>::def_f_a")]
  pub f_a: A,

  #[serde(default="S::<T>::def_f_u")]
  pub f_u: U,

  #[serde(default="S::<T>::def_f_u_1")]
  #[serde(rename="f_u1")]
  pub f_u_1: U,

  #[serde(default="S::<T>::def_f_e")]
  pub f_e: E,

  pub f_t: T,

  #[serde(default="S::<T>::def_f_bint_16")]
  #[serde(rename="f_bint16")]
  pub f_bint_16: B<i16>,

  #[serde(default="S::<T>::def_f_smap")]
  pub f_smap: std::collections::HashMap<String,i32>,

  #[serde(default="S::<T>::def_f_json_1")]
  #[serde(rename="f_json1")]
  pub f_json_1: serde_json::Value,

  #[serde(default="S::<T>::def_f_json_2")]
  #[serde(rename="f_json2")]
  pub f_json_2: serde_json::Value,

  #[serde(default="S::<T>::def_f_nt")]
  pub f_nt: Option<T>,
}

impl<T> S<T> {
  pub fn new(f_t: T) -> S<T> {
    S {
      f_void: S::<T>::def_f_void(),
      f_bool: S::<T>::def_f_bool(),
      f_int_8: S::<T>::def_f_int_8(),
      f_int_16: S::<T>::def_f_int_16(),
      f_int_32: S::<T>::def_f_int_32(),
      f_int_64: S::<T>::def_f_int_64(),
      f_word_8: S::<T>::def_f_word_8(),
      f_word_16: S::<T>::def_f_word_16(),
      f_word_32: S::<T>::def_f_word_32(),
      f_word_64: S::<T>::def_f_word_64(),
      f_float: S::<T>::def_f_float(),
      f_double: S::<T>::def_f_double(),
      f_bytes: S::<T>::def_f_bytes(),
      f_string: S::<T>::def_f_string(),
      f_vstring: S::<T>::def_f_vstring(),
      f_a: S::<T>::def_f_a(),
      f_u: S::<T>::def_f_u(),
      f_u_1: S::<T>::def_f_u_1(),
      f_e: S::<T>::def_f_e(),
      f_t: f_t,
      f_bint_16: S::<T>::def_f_bint_16(),
      f_smap: S::<T>::def_f_smap(),
      f_json_1: S::<T>::def_f_json_1(),
      f_json_2: S::<T>::def_f_json_2(),
      f_nt: S::<T>::def_f_nt(),
    }
  }

  pub fn def_f_void() -> () {
    ()
  }

  pub fn def_f_bool() -> bool {
    true
  }

  pub fn def_f_int_8() -> i8 {
    -5_i8
  }

  pub fn def_f_int_16() -> i16 {
    -10000_i16
  }

  pub fn def_f_int_32() -> i32 {
    56_i32
  }

  pub fn def_f_int_64() -> i64 {
    40000_i64
  }

  pub fn def_f_word_8() -> u8 {
    32_u8
  }

  pub fn def_f_word_16() -> u16 {
    50000_u16
  }

  pub fn def_f_word_32() -> u32 {
    124456_u32
  }

  pub fn def_f_word_64() -> u64 {
    2344_u64
  }

  pub fn def_f_float() -> f32 {
    0.5_f32
  }

  pub fn def_f_double() -> f64 {
    0.45_f64
  }

  pub fn def_f_bytes() -> Vec<u8> {
    base64::decode("aGVsbG8=").unwrap()
  }

  pub fn def_f_string() -> String {
    "abcd".to_string()
  }

  pub fn def_f_vstring() -> Vec<String> {
    vec!["xy".to_string(), "ab".to_string()]
  }

  pub fn def_f_a() -> A {
    A{f_int : 0_i16, f_string : "xyz".to_string(), f_bool : false}
  }

  pub fn def_f_u() -> U {
    U::FInt(45_i16)
  }

  pub fn def_f_u_1() -> U {
    U::FVoid
  }

  pub fn def_f_e() -> E {
    E::V2
  }

  pub fn def_f_bint_16() -> B<i16> {
    B::<i16>{f_t : 56_i16, f_string : "yikes".to_string(), f_tvec : vec![1_i16, 2_i16, 3_i16], f_xy : XY::<i16>{x : 5_i16, y : 5_i16}}
  }

  pub fn def_f_smap() -> std::collections::HashMap<String,i32> {
    [("a".to_string(), 45_i32), ("b".to_string(), 47_i32)].iter().cloned().collect()
  }

  pub fn def_f_json_1() -> serde_json::Value {
    serde_json::from_str("null").unwrap()
  }

  pub fn def_f_json_2() -> serde_json::Value {
    serde_json::from_str("[{\"v1\":27,\"v2\":\"abcde\"},true]").unwrap()
  }

  pub fn def_f_nt() -> Option<T> {
    None
  }
}
