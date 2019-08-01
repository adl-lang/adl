// @generated from adl module sys.types

use serde::ser::Serialize;
use serde::ser::SerializeStruct;
use serde::ser::Serializer;

pub struct Pair<T1, T2> {
  pub v_1: T1,
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

impl<T1: Serialize, T2: Serialize> Serialize for Pair<T1, T2> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Pair", 2)?;
    s.serialize_field("v1", &self.v_1)?;
    s.serialize_field("v2", &self.v_2)?;
    s.end()
  }
}

pub enum Either<T1, T2> {
  Left(T1),
  Right(T2),
}

pub enum Maybe<T> {
  Nothing,
  Just(T),
}

pub enum Error<T> {
  Value(T),
  Error(String),
}

pub struct Map<K, V>(pub Vec<Pair<K, V>>);

pub struct Set<T>(pub Vec<T>);
