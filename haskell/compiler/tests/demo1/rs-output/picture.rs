// @generated from adl module picture
pub mod picture {

pub enum Picture {
  Circle(Circle),
  Rectangle(Rectangle),
  Composed(Vec<Picture>),
  Translated(Box<Translated<Picture>>),
}

pub struct Circle {
  radius: f64,
}

pub struct Rectangle {
  width: f64,
  height: f64,
}

pub struct Translated<T> {
  xoffset: f64,
  yoffset: f64,
  object: T,
}
}
