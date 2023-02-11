use super::RustOpts;
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

    let writer = TreeWriter::new(opts.output.outdir.clone());

    for m in modules {
        let path = path_from_module_name(m.name.to_owned());
        let code = generate_code(m).unwrap();
        writer.write(path.as_path(), code)?;
    }

    Ok(())
}

fn path_from_module_name(mname: adlast::ModuleName) -> PathBuf {
    let mut path = PathBuf::new();
    for el in mname.split(".") {
        path.push(el);
    }
    path.set_extension("rs");
    return path;
}

fn generate_code(m: &Module1) -> anyhow::Result<String> {
    let mut out = String::new();

    for d in m.decls.values() {
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
