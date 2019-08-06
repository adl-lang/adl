// @generated from adl module test5

use serde::Deserialize;
use serde::Serialize;

#[derive(Serialize,Deserialize)]
pub enum U1 {
  #[serde(rename="v")]
  V,
}

#[derive(Serialize,Deserialize)]
pub enum U2 {
  #[serde(rename="v")]
  V(i16),
}

#[derive(Serialize,Deserialize)]
pub enum U3 {
  #[serde(rename="v")]
  V(i16),
}

#[derive(Serialize,Deserialize)]
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

#[derive(Serialize,Deserialize)]
pub enum U4 {
  #[serde(rename="v")]
  V(S1),
}

#[derive(Serialize,Deserialize)]
pub enum U5 {
  #[serde(rename="v")]
  V(S1),
}

#[derive(Serialize,Deserialize)]
pub enum U6 {
  #[serde(rename="v")]
  V(U3),
}

#[derive(Serialize,Deserialize)]
pub enum U7 {
  #[serde(rename="v")]
  V(U3),
}

#[derive(Serialize,Deserialize)]
pub enum U8 {
  #[serde(rename="v1")]
  V1(S1),

  #[serde(rename="v2")]
  V2(i16),
}

#[derive(Serialize,Deserialize)]
pub enum U9<T> {
  #[serde(rename="v1")]
  V1(T),

  #[serde(rename="v2")]
  V2(i16),
}

#[derive(Serialize,Deserialize)]
pub enum List<T> {
  #[serde(rename="null")]
  Null,

  #[serde(rename="cell")]
  Cell(Cell<T>),
}

#[derive(Serialize,Deserialize)]
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
