use nom_locate::LocatedSpan;
use std::fs;
use std::io::ErrorKind;
use std::path::PathBuf;

use crate::adlgen::sys::adlast2 as adlast;
use crate::parser::{process_parse_error, raw_module};
use crate::processing::annotations::apply_explicit_annotations;

use super::Module0;

pub trait AdlLoader {
    /// Find and load the specified ADL module
    fn load(&mut self, module_name: &adlast::ModuleName) -> Result<Option<Module0>, anyhow::Error>;
}

/// Combines a bunch of loaders
pub struct MultiLoader {
    loaders: Vec<Box<dyn AdlLoader>>,
}

impl MultiLoader {
    pub fn new(loaders: Vec<Box<dyn AdlLoader>>) -> Self {
        MultiLoader { loaders }
    }
}

impl AdlLoader for MultiLoader {
    fn load(&mut self, module_name: &adlast::ModuleName) -> Result<Option<Module0>, anyhow::Error> {
        for loader in &mut self.loaders {
            if let Some(module) = loader.load(module_name)? {
                return Ok(Some(module));
            }
        }
        Ok(None)
    }
}

pub struct DirTreeLoader {
    root: PathBuf,
}

impl DirTreeLoader {
    pub fn new(root: PathBuf) -> Self {
        DirTreeLoader { root }
    }
}

impl AdlLoader for DirTreeLoader {
    fn load(&mut self, module_name: &adlast::ModuleName) -> Result<Option<Module0>, anyhow::Error> {
        let mut path = self.root.clone();
        for mp in module_name.split(".") {
            path.push(mp);
        }
        path.set_extension("adl");
        let econtent = fs::read_to_string(path);
        let content = match econtent {
            Err(err) => match err.kind() {
                ErrorKind::NotFound => return Ok(None),
                _ => return Err(anyhow::Error::from(err)),
            },
            Ok(content) => content,
        };
        parse(&content).map(|m| Some(m))
    }
}

fn parse(content: &str) -> Result<Module0, anyhow::Error> {
    let inp = LocatedSpan::new(content);
    let (_, raw_module) = raw_module(inp).map_err(process_parse_error)?;
    match apply_explicit_annotations(raw_module) {
        Ok(module0) => Ok(module0),
        Err(err) => Err(anyhow::Error::from(err)),
    }
}
