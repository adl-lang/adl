// @generated from adl module test7

use std::marker::PhantomData;
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

impl Serialize for Int2 {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let Int2(value) = self;
    value.serialize(serializer)
  }
}

pub struct Int3(pub i64);

impl Serialize for Int3 {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let Int3(value) = self;
    value.serialize(serializer)
  }
}

pub type Int4 = i64;

pub struct Int5<X>(pub i64, PhantomData<X>);

impl<X: Serialize> Serialize for Int5<X> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let Int5(value) = self;
    value.serialize(serializer)
  }
}

pub struct Int6<X>(pub i64, PhantomData<X>);

impl<X: Serialize> Serialize for Int6<X> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let Int6(value) = self;
    value.serialize(serializer)
  }
}

pub type String1 = String;

pub struct String2(pub String);

impl Serialize for String2 {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let String2(value) = self;
    value.serialize(serializer)
  }
}

pub struct String3(pub String);

impl Serialize for String3 {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let String3(value) = self;
    value.serialize(serializer)
  }
}

pub type String4 = String;

pub struct String5<X>(pub String, PhantomData<X>);

impl<X: Serialize> Serialize for String5<X> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let String5(value) = self;
    value.serialize(serializer)
  }
}

pub struct String6<X>(pub String, PhantomData<X>);

impl<X: Serialize> Serialize for String6<X> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let String6(value) = self;
    value.serialize(serializer)
  }
}

pub type IntPoint1 = Point<i64>;

pub struct IntPoint2(pub Point<i64>);

impl Serialize for IntPoint2 {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let IntPoint2(value) = self;
    value.serialize(serializer)
  }
}

pub struct IntPoint3(pub Point<i64>);

impl Serialize for IntPoint3 {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let IntPoint3(value) = self;
    value.serialize(serializer)
  }
}

pub type Point1<X> = Point<X>;

pub struct Point2<X>(pub Point<X>);

impl<X: Serialize> Serialize for Point2<X> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let Point2(value) = self;
    value.serialize(serializer)
  }
}

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
