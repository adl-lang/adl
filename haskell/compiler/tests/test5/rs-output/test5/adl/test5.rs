// @generated from adl module test5

use serde::Deserialize;
use serde::Serialize;

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U1 {
  #[serde(rename="v")]
  V,
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U2 {
  #[serde(rename="v")]
  V(i16),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U3 {
  #[serde(rename="v")]
  V(i16),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct S1 {
  #[serde(default="S1::def_f")]
  pub f: i16,
}

impl S1 {
  pub fn new() -> S1 {
    S1 {
      f: S1::def_f(),
    }
  }

  pub fn def_f() -> i16 {
    100_i16
  }
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U4 {
  #[serde(rename="v")]
  V(S1),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U5 {
  #[serde(rename="v")]
  V(S1),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U6 {
  #[serde(rename="v")]
  V(U3),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U7 {
  #[serde(rename="v")]
  V(U3),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U8 {
  #[serde(rename="v1")]
  V1(S1),

  #[serde(rename="v2")]
  V2(i16),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U9<T> {
  #[serde(rename="v1")]
  V1(T),

  #[serde(rename="v2")]
  V2(i16),

  #[serde(rename="v3")]
  V3,
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct S {
  #[serde(default="S::def_f_1")]
  #[serde(rename="f1")]
  pub f_1: U9<String>,

  #[serde(default="S::def_f_2")]
  #[serde(rename="f2")]
  pub f_2: U9<String>,

  #[serde(default="S::def_f_3")]
  #[serde(rename="f3")]
  pub f_3: U9<String>,
}

impl S {
  pub fn new() -> S {
    S {
      f_1: S::def_f_1(),
      f_2: S::def_f_2(),
      f_3: S::def_f_3(),
    }
  }

  pub fn def_f_1() -> U9<String> {
    U9::V1("xx".to_string())
  }

  pub fn def_f_2() -> U9<String> {
    U9::V2(100_i16)
  }

  pub fn def_f_3() -> U9<String> {
    U9::V3
  }
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum List<T> {
  #[serde(rename="null")]
  Null,

  #[serde(rename="cell")]
  Cell(Cell<T>),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Cell<T> {
  pub head: T,

  pub tail: Box<List<T>>,
}

impl<T> Cell<T> {
  pub fn new(head: T, tail: Box<List<T>>) -> Cell<T> {
    Cell {
      head: head,
      tail: tail,
    }
  }
}
