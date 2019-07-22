// @generated from adl module test7
pub mod test7 {

pub struct Point<T> {
  x: T,
  y: T,
}

pub type Int1 = i64;

struct Int2(pub i64);

struct Int3(pub i64);

pub type Int4 = i64;

struct Int5<X>(pub i64, std::marker::PhantomData<X>);

struct Int6<X>(pub i64, std::marker::PhantomData<X>);

pub type String1 = String;

struct String2(pub String);

struct String3(pub String);

pub type String4 = String;

struct String5<X>(pub String, std::marker::PhantomData<X>);

struct String6<X>(pub String, std::marker::PhantomData<X>);

pub type IntPoint1 = Point<i64>;

struct IntPoint2(pub Point<i64>);

struct IntPoint3(pub Point<i64>);

pub type Point1<X> = Point<X>;

struct Point2<X>(pub Point<X>);

pub type IntPoint1A = IntPoint1;

pub struct S {
  f1: IntPoint1A,
}
}
