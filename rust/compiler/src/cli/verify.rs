use std::path::PathBuf;

use super::VerifyOpts;

use crate::processing::loader::{MultiLoader, DirTreeLoader, AdlLoader};

pub fn verify(opts: &VerifyOpts) {
  let mut loader = loader_from_search_paths(&opts.searchdir);
  for m in &opts.modules {
    let lm = loader.load(m);
    match lm {
      Ok(Some(_)) => println!("Verified module {}", m),
      Ok(None) => println!("Failed to find module {}", m),
      Err(e) => println!("Failed to verify module {}: {}", m, e),

    }
  }
}

pub fn loader_from_search_paths(paths: &Vec<PathBuf>) -> Box<dyn AdlLoader> {
  let loaders = paths.iter().map(loader_from_dir_tree).collect();
  Box::new(MultiLoader::new(loaders))
}

pub fn loader_from_dir_tree(path: &PathBuf) -> Box<dyn AdlLoader> {
Box::new(DirTreeLoader::new(path.clone()))
}