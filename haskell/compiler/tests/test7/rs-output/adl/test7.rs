// @generated from adl module test7

pub struct Point<T> {
  pub x: T,
  pub y: T,
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
