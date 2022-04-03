// @generated from adl module test5

use serde::Deserialize;
use serde::Serialize;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U1 {
  #[serde(rename="v")]
  V,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U2 {
  #[serde(rename="v")]
  V(i16),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U3 {
  #[serde(rename="v")]
  V(i16),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
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

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U4 {
  #[serde(rename="v")]
  V(S1),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U5 {
  #[serde(rename="v")]
  V(S1),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U6 {
  #[serde(rename="v")]
  V(U3),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U7 {
  #[serde(rename="v")]
  V(U3),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U8 {
  #[serde(rename="v1")]
  V1(S1),

  #[serde(rename="v2")]
  V2(i16),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U9<T> {
  #[serde(rename="v1")]
  V1(T),

  #[serde(rename="v2")]
  V2(i16),

  #[serde(rename="v3")]
  V3,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
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

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum List<T> {
  #[serde(rename="null")]
  Null,

  #[serde(rename="cell")]
  Cell(Cell<T>),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
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

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U10 {
  #[serde(rename="v1")]
  V1(i16),

  #[serde(rename="v2")]
  V2,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct S10 {
  #[serde(default="S10::def_f_1")]
  #[serde(rename="f1")]
  pub f_1: U10,

  #[serde(default="S10::def_f_2")]
  #[serde(rename="f2")]
  pub f_2: Option<U10>,

  #[serde(default="S10::def_f_3")]
  #[serde(rename="f3")]
  pub f_3: U10,

  #[serde(default="S10::def_f_4")]
  #[serde(rename="f4")]
  pub f_4: Option<U10>,
}

impl S10 {
  pub fn new() -> S10 {
    S10 {
      f_1: S10::def_f_1(),
      f_2: S10::def_f_2(),
      f_3: S10::def_f_3(),
      f_4: S10::def_f_4(),
    }
  }

  pub fn def_f_1() -> U10 {
    U10::V2
  }

  pub fn def_f_2() -> Option<U10> {
    Some(U10::V2)
  }

  pub fn def_f_3() -> U10 {
    U10::V1(17_i16)
  }

  pub fn def_f_4() -> Option<U10> {
    Some(U10::V1(17_i16))
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum U11 {
  #[serde(rename="VALUE1")]
  V1(i16),

  #[serde(rename="VALUE2")]
  V2,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct S11 {
  #[serde(default="S11::def_f_1")]
  #[serde(rename="f1")]
  pub f_1: U11,

  #[serde(default="S11::def_f_2")]
  #[serde(rename="f2")]
  pub f_2: Option<U11>,

  #[serde(default="S11::def_f_3")]
  #[serde(rename="f3")]
  pub f_3: U11,

  #[serde(default="S11::def_f_4")]
  #[serde(rename="f4")]
  pub f_4: Option<U11>,
}

impl S11 {
  pub fn new() -> S11 {
    S11 {
      f_1: S11::def_f_1(),
      f_2: S11::def_f_2(),
      f_3: S11::def_f_3(),
      f_4: S11::def_f_4(),
    }
  }

  pub fn def_f_1() -> U11 {
    U11::V2
  }

  pub fn def_f_2() -> Option<U11> {
    Some(U11::V2)
  }

  pub fn def_f_3() -> U11 {
    U11::V1(17_i16)
  }

  pub fn def_f_4() -> Option<U11> {
    Some(U11::V1(17_i16))
  }
}
