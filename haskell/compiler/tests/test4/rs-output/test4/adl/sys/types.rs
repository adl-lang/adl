// @generated from adl module sys.types

use serde::Deserialize;
use serde::Serialize;

#[derive(Serialize,Deserialize)]
pub struct Pair<T1, T2> {
  #[serde(rename="v1")]
  pub v_1: T1,

  #[serde(rename="v2")]
  pub v_2: T2,
}

impl<T1, T2> Pair<T1, T2> {
  pub fn new(v_1: T1, v_2: T2) -> Pair<T1, T2> {
    Pair {
      v_1: v_1,
      v_2: v_2,
    }
  }
}

#[derive(Serialize,Deserialize)]
pub enum Either<T1, T2> {
  #[serde(rename="left")]
  Left(T1),

  #[serde(rename="right")]
  Right(T2),
}

#[derive(Serialize,Deserialize)]
pub enum Maybe<T> {
  #[serde(rename="nothing")]
  Nothing,

  #[serde(rename="just")]
  Just(T),
}

#[derive(Serialize,Deserialize)]
pub enum Error<T> {
  #[serde(rename="value")]
  Value(T),

  #[serde(rename="error")]
  Error(String),
}

#[derive(Serialize,Deserialize)]
pub struct Map<K, V>(pub Vec<Pair<K, V>>);

#[derive(Serialize,Deserialize)]
pub struct Set<T>(pub Vec<T>);
