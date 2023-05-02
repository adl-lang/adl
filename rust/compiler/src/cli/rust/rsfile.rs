use std::collections::{BTreeSet};


/// In memory representation of a rust file during codegen
pub struct RSFile {
  module_name: String,
  body: Vec<String>,
  imports: BTreeSet<RustScopedImport>,
  imported_names: BTreeSet<String>,

}

impl RSFile {
  pub fn new(module_name: String) -> Self {
      RSFile{
          module_name,
          body: Vec::new(),
          imports: BTreeSet::new(),
          imported_names: BTreeSet::new(),
      }
  }

  pub fn pushln(&mut self, text: String ) {
      self.body.push(text);
  }

  pub fn pushlns(&mut self, lines: Vec<String> ) {
    for l in lines {
      self.body.push(l);
    }
  }

  pub fn import(&mut self, rs_module: &str, rs_name: &str) -> String {
    let rsi = RustScopedImport{
      rs_module: rs_module.to_owned(),
      rs_name: rs_name.to_owned(),
    };
    if self.imports.contains(&rsi) {
      // Use the already imported local name
      rs_name.to_owned()
    } else {
      if self.imported_names.contains(rs_name) {
        // Use the fully scoped name as the local name is already in use
        format!("{}::{}", rs_module, rs_name)
      } else {
        // Add the import
        self.imported_names.insert(rs_name.to_owned());
        self.imports.insert(rsi);
        rs_name.to_owned()
      }
    }
  }

  pub fn to_string(&self) -> String {
    let mut lines = Vec::new();
    lines.push(format!("// @generated from adl module {}",self.module_name));
    lines.push("".to_owned());
    for rsi in &self.imports {
      lines.push(format!("use {}::{};", rsi.rs_module, rsi.rs_name));
    }
    lines.push("".to_owned());
    for l in &self.body {
      lines.push(l.clone())
    }
    lines.join("\n")
  }
}

#[macro_export]
macro_rules! fmtln {
  ($dst:expr, $($arg:tt)*) => {
      $dst.pushln(format!($($arg)*))
  };
}

#[derive(Eq,PartialEq,Ord,PartialOrd)]
struct RustScopedImport {
  rs_module: String,
  rs_name: String,
}