// @generated from adl module test4

use crate::adlrt::custom::sys::types::map::Map;
use crate::adlrt::custom::sys::types::mapentry::MapEntry;
use crate::adlrt::custom::sys::types::maybe::Maybe;
use crate::adlrt::custom::sys::types::pair::Pair;
use crate::adlrt::custom::sys::types::set::Set;
use crate::test4::customtypes::CDate;
use crate::test4::customtypes::Date;
use crate::test4::customtypes::cdatehelpers;
use crate::test4::customtypes::datehelpers;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct CDate0 {
  pub year: i16,

  pub month: i16,

  pub day: i16,
}

impl CDate0 {
  pub fn new(year: i16, month: i16, day: i16) -> CDate0 {
    CDate0 {
      year: year,
      month: month,
      day: day,
    }
  }
}

#[derive(Clone,Deserialize,Eq,PartialEq,Serialize)]
pub struct S {
  #[serde(rename="v1")]
  pub v_1: Date,

  #[serde(default="S::def_v_2")]
  #[serde(rename="v2")]
  pub v_2: Date,

  #[serde(rename="v3")]
  pub v_3: CDate,

  #[serde(default="S::def_v_4")]
  #[serde(rename="v4")]
  pub v_4: CDate,

  #[serde(rename="v5")]
  pub v_5: Maybe<String>,

  #[serde(default="S::def_v_5_a")]
  #[serde(rename="v5a")]
  pub v_5_a: Maybe<String>,

  #[serde(default="S::def_v_5_b")]
  #[serde(rename="v5b")]
  pub v_5_b: Maybe<String>,

  #[serde(rename="v6")]
  pub v_6: Pair<String, i32>,

  #[serde(default="S::def_v_7")]
  #[serde(rename="v7")]
  pub v_7: Set<i32>,

  #[serde(rename="v7a")]
  pub v_7_a: Set<i32>,

  #[serde(rename="v8")]
  pub v_8: Map<String, i32>,

  #[serde(default="S::def_v_8_a")]
  #[serde(rename="v8a")]
  pub v_8_a: Map<String, i32>,
}

impl S {
  pub fn new(v_1: Date, v_3: CDate, v_5: Maybe<String>, v_6: Pair<String, i32>, v_7_a: Set<i32>, v_8: Map<String, i32>) -> S {
    S {
      v_1: v_1,
      v_2: S::def_v_2(),
      v_3: v_3,
      v_4: S::def_v_4(),
      v_5: v_5,
      v_5_a: S::def_v_5_a(),
      v_5_b: S::def_v_5_b(),
      v_6: v_6,
      v_7: S::def_v_7(),
      v_7_a: v_7_a,
      v_8: v_8,
      v_8_a: S::def_v_8_a(),
    }
  }

  pub fn def_v_2() -> Date {
    datehelpers::new("2000-01-01".to_string())
  }

  pub fn def_v_4() -> CDate {
    cdatehelpers::new(2000_i16, 1_i16, 1_i16)
  }

  pub fn def_v_5_a() -> Maybe<String> {
    Maybe::nothing()
  }

  pub fn def_v_5_b() -> Maybe<String> {
    Maybe::just("hello".to_string())
  }

  pub fn def_v_7() -> Set<i32> {
    Set::new(vec![1_i32, 2_i32, 3_i32])
  }

  pub fn def_v_8_a() -> Map<String, i32> {
    Map::new(vec![MapEntry::new("X".to_string(), 1_i32), MapEntry::new("Y".to_string(), 2_i32)])
  }
}
