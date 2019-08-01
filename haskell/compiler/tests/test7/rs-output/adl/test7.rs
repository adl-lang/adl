// @generated from adl module test7

use serde::ser::Serialize;
use serde::ser::SerializeStruct;
use serde::ser::Serializer;

pub struct Point<T> {
  pub x: T,
  pub y: T,
}

impl<T> Point<T> {
  pub fn new(x: T, y: T) -> Point<T> {
    Point {
      x: x,
      y: y,
    }
  }
}

impl<T: Serialize> Serialize for Point<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Point", 2)?;
    s.serialize_field("x", &self.x)?;
    s.serialize_field("y", &self.y)?;
    s.end()
  }
}

pub type Int1 = i64;

pub struct Int2(pub i64);

pub struct Int3(pub i64);

pub type Int4 = i64;

pub struct Int5<X>(pub i64, std::marker::PhantomData<X>);

pub struct Int6<X>(pub i64, std::marker::PhantomData<X>);

pub type String1 = String;

pub struct String2(pub String);

pub struct String3(pub String);

pub type String4 = String;

pub struct String5<X>(pub String, std::marker::PhantomData<X>);

pub struct String6<X>(pub String, std::marker::PhantomData<X>);

pub type IntPoint1 = Point<i64>;

pub struct IntPoint2(pub Point<i64>);

pub struct IntPoint3(pub Point<i64>);

pub type Point1<X> = Point<X>;

pub struct Point2<X>(pub Point<X>);

pub type IntPoint1A = IntPoint1;

pub struct S {
  pub f_1: IntPoint1A,
}

impl S {
  pub fn new(f_1: IntPoint1A) -> S {
    S {
      f_1: f_1,
    }
  }
}

impl Serialize for S {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("S", 1)?;
    s.serialize_field("f1", &self.f_1)?;
    s.end()
  }
}
