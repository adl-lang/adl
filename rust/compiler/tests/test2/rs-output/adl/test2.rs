// @generated from adl module test2

use serde::Deserialize;
use serde::Serialize;

/**
 * An empty structure.
 */
#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct S0 {
}

impl S0 {
  pub fn new() -> S0 {
    S0 {
    }
  }
}

/**
 * A structure containing primitives.
 * It has two fields: an integer x and a String y.
 */
#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct S1 {
  pub x: i32,

  pub y: String,
}

impl S1 {
  pub fn new(x: i32, y: String) -> S1 {
    S1 {
      x: x,
      y: y,
    }
  }
}

/**
 * A structure containing a vector.
 */
#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct S2 {
  #[serde(rename="f1")]
  pub f_1: String,

  #[serde(rename="f2")]
  pub f_2: f64,

  #[serde(rename="f3")]
  pub f_3: Vec<i32>,
}

impl S2 {
  pub fn new(f_1: String, f_2: f64, f_3: Vec<i32>) -> S2 {
    S2 {
      f_1: f_1,
      f_2: f_2,
      f_3: f_3,
    }
  }
}

/**
 * A generic structure.
 */
#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct S3<T> {
  #[serde(rename="f1")]
  pub f_1: String,

  #[serde(rename="f2")]
  pub f_2: f64,

  #[serde(rename="f3")]
  pub f_3: T,

  #[serde(rename="f4")]
  pub f_4: Vec<T>,
}

impl<T> S3<T> {
  pub fn new(f_1: String, f_2: f64, f_3: T, f_4: Vec<T>) -> S3<T> {
    S3 {
      f_1: f_1,
      f_2: f_2,
      f_3: f_3,
      f_4: f_4,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct S4<T> {
  #[serde(rename="f1")]
  pub f_1: S3<String>,

  #[serde(rename="f2")]
  pub f_2: S3<T>,
}

impl<T> S4<T> {
  pub fn new(f_1: S3<String>, f_2: S3<T>) -> S4<T> {
    S4 {
      f_1: f_1,
      f_2: f_2,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Tree<T> {
  pub value: T,

  pub children: Vec<Tree<T>>,
}

impl<T> Tree<T> {
  pub fn new(value: T, children: Vec<Tree<T>>) -> Tree<T> {
    Tree {
      value: value,
      children: children,
    }
  }
}

pub type IntTree = Tree<i32>;
