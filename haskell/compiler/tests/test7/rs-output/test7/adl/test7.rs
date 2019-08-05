// @generated from adl module test7

use serde::Deserialize;
use std::marker::PhantomData;
use serde::Serialize;

#[derive(Serialize,Deserialize)]
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

#[derive(Serialize,Deserialize)]
pub struct Int2(pub i64);

#[derive(Serialize,Deserialize)]
pub struct Int3(pub i64);

pub type Int4 = i64;

#[derive(Serialize,Deserialize)]
pub struct Int5<X>(pub i64, PhantomData<X>);

#[derive(Serialize,Deserialize)]
pub struct Int6<X>(pub i64, PhantomData<X>);

pub type String1 = String;

#[derive(Serialize,Deserialize)]
pub struct String2(pub String);

#[derive(Serialize,Deserialize)]
pub struct String3(pub String);

pub type String4 = String;

#[derive(Serialize,Deserialize)]
pub struct String5<X>(pub String, PhantomData<X>);

#[derive(Serialize,Deserialize)]
pub struct String6<X>(pub String, PhantomData<X>);

pub type IntPoint1 = Point<i64>;

#[derive(Serialize,Deserialize)]
pub struct IntPoint2(pub Point<i64>);

#[derive(Serialize,Deserialize)]
pub struct IntPoint3(pub Point<i64>);

pub type Point1<X> = Point<X>;

#[derive(Serialize,Deserialize)]
pub struct Point2<X>(pub Point<X>);

pub type IntPoint1A = IntPoint1;

#[derive(Serialize,Deserialize)]
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
