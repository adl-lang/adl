use super::RustOpts;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;


use crate::adlgen::sys::adlast2::{self as adlast, PrimitiveType};
use crate::processing::resolver::{Module1, Resolver, TypeExpr1};
use crate::processing::writer::TreeWriter;

use crate::cli::rust::rsfile::RSFile;
use crate::fmtln;


/// Generate the tree of mod.rs files that link together the generated code
pub fn gen_rs_mod_files(opts: &RustOpts, resolver: &Resolver, writer: &mut TreeWriter) -> anyhow::Result<()> {

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

pub fn gen_module(m: &Module1) -> anyhow::Result<String> {
    let mut out = RSFile::new();

    for d in m.decls.iter() {
        match &d.r#type {
            adlast::DeclType::Struct(s) => gen_struct(m, d, &s, &mut out)?,
            _ => {}
        }
    }
    Ok(out.to_string())
}


fn gen_struct(
    _m: &Module1,
    d: &adlast::Decl<TypeExpr1>,
    s: &adlast::Struct<TypeExpr1>,
    out: &mut RSFile,
) -> anyhow::Result<()> {
    fmtln!(out, "pub struct {} {{", d.name);
    for f in &s.fields {
        fmtln!(out, "    {}: {};", f.name, gen_type_expr(&f.type_expr));
    }
    fmtln!(out, "}}");
    fmtln!(out, "");
    fmtln!(out, "impl {} {{", d.name);
    fmtln!(out, "  pub fn new() -> {} {{", d.name);
    fmtln!(out, "    {} {{", d.name);
    fmtln!(out, "    }}");
    fmtln!(out, "  }}");
    fmtln!(out, "}}");
    fmtln!(out, "");
    Ok(())
}

fn gen_type_expr(te: &TypeExpr1) -> String {
    match &te.type_ref {
        adlast::TypeRef::LocalName(_ln) => "UNIMP.LOCAL_NAME".to_owned(),
        adlast::TypeRef::ScopedName(_sn) => "UNIMP.SCOPED_NAME".to_owned(),
        adlast::TypeRef::Primitive(p) => gen_primitive_type_expr(p, &te.parameters),
        adlast::TypeRef::TypeParam(_t) => "UNIMP.TYPE_PARAM".to_owned(),
    }
}

fn gen_primitive_type_expr(p: &PrimitiveType, _params: &[TypeExpr1]) -> String {
    match p {
        PrimitiveType::Void => "()".to_owned(),
        PrimitiveType::Bool => "bool".to_owned(),
        PrimitiveType::Int8 => "i8".to_owned(),
        PrimitiveType::Int16 => "i16".to_owned(),
        PrimitiveType::Int32 => "i32".to_owned(),
        PrimitiveType::Int64 => "i64".to_owned(),
        PrimitiveType::Word8 => "u8".to_owned(),
        PrimitiveType::Word16 => "u16".to_owned(),
        PrimitiveType::Word32 => "u32".to_owned(),
        PrimitiveType::Word64 => "u64".to_owned(),
        PrimitiveType::Float => "f32".to_owned(),
        PrimitiveType::Double => "f64".to_owned(),
        PrimitiveType::Json => "UNIMP_JSON".to_owned(),
        PrimitiveType::ByteVector => "Vec<u8>".to_owned(),
        PrimitiveType::String => "String".to_owned(),
        PrimitiveType::Vector => "UNIMP_VECTOR".to_owned(),
        PrimitiveType::StringMap => "UNIMP_STRINGMAP".to_owned(),
        PrimitiveType::Nullable => "UNIMP_NULLABLE".to_owned(),
        PrimitiveType::TypeToken => "UNIMP_TYPETOKEN".to_owned(),
    }
}
