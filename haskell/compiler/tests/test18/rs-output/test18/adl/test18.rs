// @generated from adl module test18

use serde::Deserialize;
use serde::Serialize;

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub enum X1 {
  #[serde(rename="f1")]
  F1(f64),

  #[serde(rename="f2")]
  F2(Box<Y1>),
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub enum Y1 {
  #[serde(rename="f1")]
  F1(String),

  #[serde(rename="f2")]
  F2(Box<X1>),
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct X2 {
  #[serde(rename="f1")]
  pub f_1: f64,

  #[serde(rename="f2")]
  pub f_2: Vec<Y2>,
}

impl X2 {
  pub fn new(f_1: f64, f_2: Vec<Y2>) -> X2 {
    X2 {
      f_1: f_1,
      f_2: f_2,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct Y2 {
  #[serde(rename="f1")]
  pub f_1: String,

  #[serde(rename="f2")]
  pub f_2: Vec<X2>,
}

impl Y2 {
  pub fn new(f_1: String, f_2: Vec<X2>) -> Y2 {
    Y2 {
      f_1: f_1,
      f_2: f_2,
    }
  }
}
