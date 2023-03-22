use super::RustOpts;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use convert_case::{Case, Casing};


use crate::adlgen::sys::adlast2::{self as adlast, PrimitiveType};
use crate::processing::resolver::{Module1, Resolver, TypeExpr1, TypeRef};
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
    let mut out = RSFile::new(m.name.clone());
    out.import("serde", "Deserialize");
    out.import("serde", "Serialize");

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
    let efields: Vec<ExtendedField> = s.fields
    .iter()
    .map(|field| ExtendedField::new(field, out))
    .collect();

    let ctor_params: String = efields
    .iter()
    .map(|ef| format!("{}: {}", ef.rs_field_name, ef.type_str))
    .collect::<Vec<String>>()
    .join(", ");

    let type_params = fmt_type_params(&s.type_params);

    if let Some(serde_json::Value::String(doc)) = d.annotations.0.get(&ann_doc()) {
      out.pushlns(fmt_doc_comment(doc));
    }

    fmtln!(out, "pub struct {}{} {{", d.name, type_params );
    for (i,ef) in efields.iter().enumerate() {
        if i != 0 {
          fmtln!(out, "");
        }
        if ef.rs_field_name != ef.field.serialized_name {
          fmtln!(out, "  #[serde(rename=\"{}\")]", ef.field.serialized_name);
        }
        fmtln!(out, "  pub {}: {},", ef.rs_field_name, ef.type_str);
    }
    fmtln!(out, "}}");
    fmtln!(out, "");
    fmtln!(out, "impl{} {}{} {{", type_params, d.name, type_params);
    fmtln!(out, "  pub fn new({}) -> {}{} {{", ctor_params, d.name, type_params);
    fmtln!(out, "    {} {{", d.name);
    for ef in &efields {
      fmtln!(out, "      {}: {},",  ef.rs_field_name,  ef.rs_field_name);
    }
    fmtln!(out, "    }}");
    fmtln!(out, "  }}");
    fmtln!(out, "}}");
    fmtln!(out, "");
    Ok(())
}

struct ExtendedField<'a> {
  field: &'a adlast::Field<TypeExpr1>,
  rs_field_name: String,
  type_str: String,
  type_expr_gen: TypeExprGen,
}

impl <'a> ExtendedField<'a> {
  fn new(field: &'a adlast::Field<TypeExpr1>, out: &mut RSFile) -> Self {
    let type_expr_gen = TypeExprGen::new(&field.type_expr);
    let type_str = type_expr_gen.gen_type(out);
    let rs_field_name = field.name.to_case(Case::Snake);
    ExtendedField{field, rs_field_name, type_str, type_expr_gen}
  }
}

trait TypeCodeGen {
  fn gen_type(&self, rsfile: &mut RSFile) -> String;
}

struct TypeExprGen {
  type_ref:  adlast::TypeRef,
  params: Vec<Box<dyn TypeCodeGen>>,
}

impl TypeExprGen {
  fn new(type_expr: &TypeExpr1) -> Self {
    let params = type_expr.parameters
        .iter()
        .map(|te| Box::new(TypeExprGen::new(te)) as Box<dyn TypeCodeGen>)
        .collect();
    TypeExprGen {
      type_ref: type_expr.type_ref.clone(),
      params,
    }
  }
}

impl TypeCodeGen for TypeExprGen {
  fn gen_type(&self, rsfile: &mut RSFile) -> String {

    let params: Vec<String> = self.params
    .iter()
    .map(|p| p.gen_type(rsfile))
    .collect();

    match &self.type_ref {
      adlast::TypeRef::LocalName(ln) => format!("{}{}", ln,fmt_type_params(&params)),
      adlast::TypeRef::ScopedName(sn) => {
        let name = rsfile.import(&sn.module_name, &sn.name);
        format!("{}{}", name, fmt_type_params(&params))
      },
      adlast::TypeRef::Primitive(p) => {
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
          PrimitiveType::Json => {
            rsfile.import("serde_json", "Value")
          },  
          PrimitiveType::ByteVector => "Vec<u8>".to_owned(),
          PrimitiveType::String => "String".to_owned(),
          PrimitiveType::Vector => format!("Vec{}", fmt_type_params(&params)),
          PrimitiveType::StringMap => {
            let hash_map = rsfile.import("std::collections", "StringMap");
            format!("{}<String, {}>", hash_map, params.join(","))
          }
          PrimitiveType::Nullable => format!("Option{}", fmt_type_params(&params)),
          PrimitiveType::TypeToken => {
            let phantom_data = rsfile.import("std::marker", "PhantomData");
            phantom_data
          }
        }
      }, 
      adlast::TypeRef::TypeParam(t) => t.to_string(),
    }
  }
}


fn fmt_type_params(params: &[String]) -> String {
  if params.len() == 0 {
    "".to_owned()
  } else {
    format!("<{}>", params.join("."))
  }
}

fn fmt_doc_comment(doc_str: &str) -> Vec<String> {
  let mut lines = Vec::new();
  lines.push("/**".to_owned());
  for s in doc_str.lines() {
    let s = s.trim();
    if s != "" {
      lines.push(format!(" * {}", s))
    }
  }
  lines.push(" */".to_owned());
  lines
}

fn ann_doc() ->  adlast::ScopedName {
  adlast::ScopedName {
    module_name: "sys.annotations".to_owned(),
    name: "Doc".to_owned(),
  }
}
