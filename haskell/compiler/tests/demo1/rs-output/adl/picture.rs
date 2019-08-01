// @generated from adl module picture

use serde::ser::Serialize;
use serde::ser::SerializeStruct;
use serde::ser::Serializer;

pub enum Picture {
  Circle(Circle),
  Rectangle(Rectangle),
  Composed(Vec<Picture>),
  Translated(Box<Translated<Picture>>),
}

impl Serialize for Picture {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    match self {
      Picture::Circle(v) => {
        let mut s = serializer.serialize_struct("Picture", 1)?;
        s.serialize_field("circle", v)?;
        s.end()
      }
      Picture::Rectangle(v) => {
        let mut s = serializer.serialize_struct("Picture", 1)?;
        s.serialize_field("rectangle", v)?;
        s.end()
      }
      Picture::Composed(v) => {
        let mut s = serializer.serialize_struct("Picture", 1)?;
        s.serialize_field("composed", v)?;
        s.end()
      }
      Picture::Translated(v) => {
        let mut s = serializer.serialize_struct("Picture", 1)?;
        s.serialize_field("translated", v)?;
        s.end()
      }
    }
  }
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

impl Serialize for Circle {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Circle", 1)?;
    s.serialize_field("radius", &self.radius)?;
    s.end()
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

impl Serialize for Rectangle {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Rectangle", 2)?;
    s.serialize_field("width", &self.width)?;
    s.serialize_field("height", &self.height)?;
    s.end()
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

impl<T: Serialize> Serialize for Translated<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
      S: Serializer,
  {
    let mut s = serializer.serialize_struct("Translated", 3)?;
    s.serialize_field("xoffset", &self.xoffset)?;
    s.serialize_field("yoffset", &self.yoffset)?;
    s.serialize_field("object", &self.object)?;
    s.end()
  }
}
