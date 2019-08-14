// @generated from adl module sys.types

use crate::stdlib::Pair;
use serde::Deserialize;
use serde::Serialize;

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct PairInternal<T1, T2> {
  #[serde(rename="v1")]
  pub v_1: T1,

  #[serde(rename="v2")]
  pub v_2: T2,
}

impl<T1, T2> PairInternal<T1, T2> {
  pub fn new(v_1: T1, v_2: T2) -> PairInternal<T1, T2> {
    PairInternal {
      v_1: v_1,
      v_2: v_2,
    }
  }
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum Either<T1, T2> {
  #[serde(rename="left")]
  Left(T1),

  #[serde(rename="right")]
  Right(T2),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum MaybeInternal<T> {
  #[serde(rename="nothing")]
  Nothing,

  #[serde(rename="just")]
  Just(T),
}

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum Error<T> {
  #[serde(rename="value")]
  Value(T),

  #[serde(rename="error")]
  Error(String),
}

#[derive(Deserialize,Serialize)]
pub struct MapInternal<K, V>(pub Vec<Pair<K, V>>);

#[derive(Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct SetInternal<T>(pub Vec<T>);
