use std::path::PathBuf;

use super::VerifyOpts;

use crate::processing::{
    loader::{AdlLoader, DirTreeLoader, MultiLoader},
    resolver::Resolver,
};

pub fn verify(opts: &VerifyOpts) {
    let loader = loader_from_search_paths(&opts.searchdir);
    let mut resolver = Resolver::new(loader);
    for m in &opts.modules {
        let r = resolver.add_module(m);
        match r {
            Ok(()) => println!("Verified module {}", m),
            Err(e) => println!("Failed to verify module {}: {:?}", m, e),
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
