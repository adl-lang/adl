// @generated from adl module test7

use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use std::marker::PhantomData;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
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

pub type Int1 = i64;

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Int2(pub i64);

impl Serialize for Int2
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for Int2
{
  fn deserialize<D>(deserializer: D) -> Result<Int2, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = i64::deserialize(deserializer)?;
      Ok(Int2(v))
  }
}

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Int3(pub i64);

impl Serialize for Int3
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for Int3
{
  fn deserialize<D>(deserializer: D) -> Result<Int3, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = i64::deserialize(deserializer)?;
      Ok(Int3(v))
  }
}

pub type Int4 = i64;

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Int5<X>(pub i64, pub PhantomData<X>);

impl<X> Serialize for Int5<X>
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de, X> Deserialize<'de> for Int5<X>
{
  fn deserialize<D>(deserializer: D) -> Result<Int5<X>, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = i64::deserialize(deserializer)?;
      Ok(Int5(v, PhantomData))
  }
}

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Int6<X>(pub i64, pub PhantomData<X>);

impl<X> Serialize for Int6<X>
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de, X> Deserialize<'de> for Int6<X>
{
  fn deserialize<D>(deserializer: D) -> Result<Int6<X>, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = i64::deserialize(deserializer)?;
      Ok(Int6(v, PhantomData))
  }
}

pub type String1 = String;

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct String2(pub String);

impl Serialize for String2
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for String2
{
  fn deserialize<D>(deserializer: D) -> Result<String2, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = String::deserialize(deserializer)?;
      Ok(String2(v))
  }
}

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct String3(pub String);

impl Serialize for String3
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for String3
{
  fn deserialize<D>(deserializer: D) -> Result<String3, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = String::deserialize(deserializer)?;
      Ok(String3(v))
  }
}

pub type String4 = String;

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct String5<X>(pub String, pub PhantomData<X>);

impl<X> Serialize for String5<X>
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de, X> Deserialize<'de> for String5<X>
{
  fn deserialize<D>(deserializer: D) -> Result<String5<X>, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = String::deserialize(deserializer)?;
      Ok(String5(v, PhantomData))
  }
}

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct String6<X>(pub String, pub PhantomData<X>);

impl<X> Serialize for String6<X>
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de, X> Deserialize<'de> for String6<X>
{
  fn deserialize<D>(deserializer: D) -> Result<String6<X>, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = String::deserialize(deserializer)?;
      Ok(String6(v, PhantomData))
  }
}

pub type IntPoint1 = Point<i64>;

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct IntPoint2(pub Point<i64>);

impl Serialize for IntPoint2
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for IntPoint2
{
  fn deserialize<D>(deserializer: D) -> Result<IntPoint2, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = Point::<i64>::deserialize(deserializer)?;
      Ok(IntPoint2(v))
  }
}

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct IntPoint3(pub Point<i64>);

impl Serialize for IntPoint3
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de> Deserialize<'de> for IntPoint3
{
  fn deserialize<D>(deserializer: D) -> Result<IntPoint3, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = Point::<i64>::deserialize(deserializer)?;
      Ok(IntPoint3(v))
  }
}

pub type Point1<X> = Point<X>;

#[derive(Clone,Eq,Hash,PartialEq)]
pub struct Point2<X>(pub Point<X>);

impl<X> Serialize for Point2<X>
  where Point<X>: Serialize
{
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
      self.0.serialize(serializer)
  }
}

impl<'de, X> Deserialize<'de> for Point2<X>
  where Point<X>: Deserialize<'de>
{
  fn deserialize<D>(deserializer: D) -> Result<Point2<X>, D::Error>
  where
      D: Deserializer<'de>,
  {
      let v = Point::<X>::deserialize(deserializer)?;
      Ok(Point2(v))
  }
}

pub type IntPoint1A = IntPoint1;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct S {
  #[serde(rename="f1")]
  pub f_1: IntPoint1A,
}

impl S {
  pub fn new(f_1: IntPoint1A) -> S {
    S {
      f_1: f_1,
    }
  }
}
