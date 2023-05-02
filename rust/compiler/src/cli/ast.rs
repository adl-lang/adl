use std::collections::{HashMap};

use super::AstOpts;

use anyhow::anyhow;
use serde::Serialize;

use crate::adlgen::sys::adlast2::{AdlAst, AstModule};
use crate::processing::loader::loader_from_search_paths;
use crate::processing::resolver::{Resolver};

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

    let mut adlast = AdlAst::new();

    resolver.get_module_names().iter().for_each(|mn| {
        let (m, _) = resolver.get_module(&mn).unwrap();
        let decls: HashMap<_, _> = m.decls.iter().map(|d| (d.name.clone(), d.clone())).collect();
        let m_ast = AstModule {
            name: m.name.clone(),
            imports: m.imports.clone(),
            decls,
            annotations: m.annotations.clone(),
        };
        adlast.insert(mn.to_string(), m_ast);
    });

    let mut buf = Vec::new();
    let formatter = serde_json::ser::PrettyFormatter::with_indent(b"    ");
    let mut ser = serde_json::Serializer::with_formatter(&mut buf, formatter);
    adlast.serialize(&mut ser).unwrap();
    println!("{}", String::from_utf8(buf).unwrap());

    Ok(())
}
