use super::AstOpts;

use anyhow::anyhow;

use crate::processing::loader::loader_from_search_paths;
use crate::processing::resolver::{Module1, Resolver};

pub fn ast(opts: &AstOpts) -> anyhow::Result<()> {
    let loader = loader_from_search_paths(&opts.search.path);
    let mut resolver = Resolver::new(loader);
    for m in &opts.modules {
        let r = resolver.add_module(m);
        match r {
            Ok(()) => (),
            Err(e) => return Err(anyhow!("Failed to load module {}: {:?}", m, e)),
        }
    }
    let modules: Vec<&Module1> = resolver
        .get_module_names()
        .into_iter()
        .map(|mn| resolver.get_module(&mn).unwrap())
        .collect();
    println!("{}", serde_json::to_string_pretty(&modules).unwrap());
    Ok(())
}
