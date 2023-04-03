use super::TsOpts;

use std::collections::HashMap;
use std::path::PathBuf;

use anyhow::anyhow;

use genco::fmt::{self, Indentation};
use genco::prelude::*;

use crate::adlgen::sys::adlast2::{self as adlast};
use crate::adlgen::sys::adlast2::{
    Module, Module1, TypeExpr, TypeRef,
};
use crate::processing::loader::loader_from_search_paths;
use crate::processing::resolver::Resolver;
use crate::processing::writer::TreeWriter;

mod astgen;
mod defaultval;
mod generate;
mod utils;
#[cfg(test)]
mod tests;


pub fn tsgen(opts: &TsOpts) -> anyhow::Result<()> {
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

    let mut writer = TreeWriter::new(opts.output.outdir.clone(), opts.output.manifest.clone())?;

    for m in modules {
        let path = path_from_module_name(opts, m.name.to_owned());
        let code = gen_ts_module(m, &resolver, opts)?;
        writer.write(path.as_path(), code)?;
    }

    Ok(())
}

fn gen_ts_module(m: &Module<TypeExpr<TypeRef>>, resolver: &Resolver, opts: &TsOpts) -> anyhow::Result<String> {
    // TODO sys.annotations::SerializedName needs to be embedded
    let tokens = &mut js::Tokens::new();
    let mut mgen = generate::TsGenVisitor {
        module: m,
        resolver: resolver,
        adlr: js::import(utils::rel_import(&m.name, &"runtime.adl".to_string()), "ADL").into_wildcard(),
        map: &mut HashMap::new(),
        opts,
    };
    mgen.gen_module(tokens, m)?;
    // let stdout = std::io::stdout();
    let mut w = fmt::IoWriter::new(Vec::<u8>::new());
    // let mut w = fmt::IoWriter::new(stdout.lock());
    let fmt = fmt::Config::from_lang::<JavaScript>();
    let fmt = fmt::Config::with_indentation(fmt, Indentation::Space(2));

    let config = js::Config::default();
    // let config = js::Config{
    //     ..Default::default()
    // };
    tokens.format_file(&mut w.as_formatter(&fmt), &config)?;
    let vector = w.into_inner();
    let code = std::str::from_utf8(&vector)?;
    // let code = tokens.to_file_string()?;
    // tokens.format_file(out, config);
    Ok(code.to_string())
}

fn path_from_module_name(_opts: &TsOpts, mname: adlast::ModuleName) -> PathBuf {
    let mut path = PathBuf::new();
    // path.push(opts.module.clone());
    for el in mname.split(".") {
        path.push(el);
    }
    path.set_extension("ts");
    return path;
}
