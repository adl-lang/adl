// @generated from adl module picture

pub enum Picture {
  Circle(Circle),
  Rectangle(Rectangle),
  Composed(Vec<Picture>),
  Translated(Box<Translated<Picture>>),
}

pub struct Circle {
  pub radius: f64,
}

impl Circle {
  pub fn new(radius: f64) -> Circle {
    Circle {
      radius: radius,
    }
  }
}

pub struct Rectangle {
  pub width: f64,
  pub height: f64,
}

impl Rectangle {
  pub fn new(width: f64, height: f64) -> Rectangle {
    Rectangle {
      width: width,
      height: height,
    }
  }
}

pub struct Translated<T> {
  pub xoffset: f64,
  pub yoffset: f64,
  pub object: T,
}

impl<T> Translated<T> {
  pub fn new(object: T) -> Translated<T> {
    Translated {
      xoffset: 0_f64,
      yoffset: 0_f64,
      object: object,
    }
  }
}
