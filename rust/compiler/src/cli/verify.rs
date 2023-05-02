use super::VerifyOpts;

use anyhow::anyhow;

use crate::processing::{loader::loader_from_search_paths, resolver::Resolver};

pub fn verify(opts: &VerifyOpts) -> anyhow::Result<()> {
    let loader = loader_from_search_paths(&opts.search.path);
    let mut resolver = Resolver::new(loader);
    for m in &opts.modules {
        let r = resolver.add_module(m);
        match r {
            Ok(()) => println!("Verified module {}", m),
            Err(e) => return Err(anyhow!("Failed to verify module {}: {:?}", m, e)),
        }
    }
    Ok(())
}
