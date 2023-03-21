
/// In memory representation of a rust file during codegen
pub struct RSFile {
  lines: Vec<String>,
}

impl RSFile {
  pub fn new() -> Self {
      RSFile{
          lines: Vec::new(),
      }
  }

  pub fn pushln(&mut self, text: String ) {
      self.lines.push(text);
  }

  pub fn to_string(&self) -> String {
      self.lines.join("\n")
  }
}

#[macro_export]
macro_rules! fmtln {
  ($dst:expr, $($arg:tt)*) => {
      $dst.pushln(format!($($arg)*))
  };
}
