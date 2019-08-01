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

impl<T1: Serialize, T2: Serialize> Serialize for Either<T1, T2> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    match self {
      Either::Left(v) => {
        let mut s = serializer.serialize_struct("Either", 1)?;
        s.serialize_field("left", v)?;
        s.end()
      }
      Either::Right(v) => {
        let mut s = serializer.serialize_struct("Either", 1)?;
        s.serialize_field("right", v)?;
        s.end()
      }
    }
  }
}

pub enum Maybe<T> {
  Nothing,
  Just(T),
}

impl<T: Serialize> Serialize for Maybe<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    match self {
      Maybe::Nothing => "nothing".serialize(serializer),
      Maybe::Just(v) => {
        let mut s = serializer.serialize_struct("Maybe", 1)?;
        s.serialize_field("just", v)?;
        s.end()
      }
    }
  }
}

pub enum Error<T> {
  Value(T),
  Error(String),
}

impl<T: Serialize> Serialize for Error<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    match self {
      Error::Value(v) => {
        let mut s = serializer.serialize_struct("Error", 1)?;
        s.serialize_field("value", v)?;
        s.end()
      }
      Error::Error(v) => {
        let mut s = serializer.serialize_struct("Error", 1)?;
        s.serialize_field("error", v)?;
        s.end()
      }
    }
  }
}

pub struct Map<K, V>(pub Vec<Pair<K, V>>);

impl<K: Serialize, V: Serialize> Serialize for Map<K, V> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let Map(value) = self;
    value.serialize(serializer)
  }
}

pub struct Set<T>(pub Vec<T>);

impl<T: Serialize> Serialize for Set<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let Set(value) = self;
    value.serialize(serializer)
  }
}
