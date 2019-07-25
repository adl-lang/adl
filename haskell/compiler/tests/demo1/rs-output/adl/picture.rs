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

pub struct Rectangle {
  pub width: f64,
  pub height: f64,
}

pub struct Translated<T> {
  pub xoffset: f64,
  pub yoffset: f64,
  pub object: T,
}
