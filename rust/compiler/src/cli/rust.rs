use super::RustOpts;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::path::PathBuf;

use anyhow::anyhow;

use crate::adlgen::sys::adlast2 as adlast;
use crate::processing::loader::loader_from_search_paths;
use crate::processing::resolver::{Module1, Resolver, TypeExpr1};
use crate::processing::writer::TreeWriter;

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
    let modules: Vec<&Module1> = resolver
        .get_module_names()
        .into_iter()
        .map(|mn| resolver.get_module(&mn).unwrap())
        .collect();

    let mut writer = TreeWriter::new(
        opts.output.outdir.clone(),
        opts.output.manifest.clone(),
    )?;

    for m in modules {
        let path = path_from_module_name(opts, m.name.to_owned());
        let code = gen_module(m).unwrap();
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

/// Generate the tree of mod.rs files that link together the generated code
fn gen_rs_mod_files(opts: &RustOpts, resolver: &Resolver, writer: &mut TreeWriter) -> anyhow::Result<()> {

    // build a map of parent rust modules and their children
    let mut modfiles: HashMap<Vec<String>,HashSet<String>> = HashMap::new();
    for m in resolver.get_module_names() {
        let msplit: Vec<&str> = m.split(".").collect();
        for i in 0..msplit.len() {
            let rsmod = msplit.get(i).unwrap();
            let parent = &msplit[0..i];
            let parent: Vec<String> = parent.iter().map(|m| m.to_string()).collect();
            let e = modfiles.entry(parent).or_default();
            e.insert(rsmod.to_string());
        }
    }

    for (rsmod,children) in modfiles {
        let mut path = PathBuf::new();
        path.push(opts.module.clone());
        for el in rsmod {
            path.push(el);
        }
        path.push("mod.rs");
        let lines: Vec<String> = children
            .iter()
            .map( |m| format!("pub mod {};", m))
            .collect();
        writer.write(&path, lines.join("\n"))?
    };
    Ok(())
}

fn gen_module(m: &Module1) -> anyhow::Result<String> {
    let mut out = String::new();

    for d in m.decls.iter() {
        match &d.r#type {
            adlast::DeclType::Struct(s) => gen_struct(m, d, &s, &mut out)?,
            _ => {}
        }
    }
    Ok(out)
}

fn gen_struct(
    _m: &Module1,
    d: &adlast::Decl<TypeExpr1>,
    s: &adlast::Struct<TypeExpr1>,
    out: &mut String,
) -> anyhow::Result<()> {
    write!(out, "struct {} {{\n", d.name)?;
    for f in &s.fields {
        write!(out, "    {}: {};\n", f.name, gen_type_expr(&f.type_expr))?;
    }
    write!(out, "}}\n\n")?;
    Ok(())
}

fn gen_type_expr(_te: &TypeExpr1) -> String {
    "TYPE".to_owned()
}
