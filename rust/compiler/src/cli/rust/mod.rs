use super::RustOpts;
use std::path::PathBuf;

use anyhow::anyhow;

use crate::adlgen::adlc::packaging::AdlPackage;
use crate::adlgen::sys::adlast2::{self as adlast};
use crate::processing::loader::loader_from_search_paths;
use crate::processing::resolver::{Module1, Resolver};
use crate::processing::writer::TreeWriter;

use generate::{gen_module, gen_rs_mod_files};

mod rsfile;
mod generate;

pub fn rust(opts: &RustOpts) -> anyhow::Result<()> {
    let loader = loader_from_search_paths(&opts.search.path);
    let mut resolver = Resolver::new(loader);
    for m in &opts.modules {
        let r = resolver.add_module(m);
        match r {
            Ok(()) => (),
            Err(e) => return Err(anyhow!("Failed to load module {}: {:?}", m, e)),
        }
    }
    let modules: Vec<(Module1, Option<&AdlPackage>)> = resolver
        .get_module_names()
        .into_iter()
        .map(|mn| resolver.get_module(&mn).unwrap())
        .collect();

    let mut writer = TreeWriter::new(
        opts.output.outputdir.clone(),
        opts.output.manifest.clone(),
    )?;

    for (m,_) in modules {
        let path = path_from_module_name(opts, m.name.to_owned());
        let code = gen_module(&m).unwrap();
        writer.write(path.as_path(), code)?;
    }

    gen_rs_mod_files(opts, &resolver, &mut writer)?;

    Ok(())
}

fn path_from_module_name(opts: &RustOpts, mname: adlast::ModuleName) -> PathBuf {
    let mut path = PathBuf::new();
    path.push(opts.module.clone());
    for el in mname.split(".") {
        path.push(el);
    }
    path.set_extension("rs");
    return path;
}
