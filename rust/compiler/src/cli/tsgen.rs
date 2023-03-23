use super::TsOpts;

use std::cmp::Ordering;
use std::collections::{HashMap};

use anyhow::anyhow;
use genco::prelude::js::Import as JsImport;
use genco::tokens::{Item, ItemStr};

use crate::adlgen::sys::adlast2::{
    Annotations, Decl, DeclType, Field, Ident, Module, NewType, PrimitiveType, ScopedName, Struct,
    TypeDef, TypeExpr, TypeRef, Union,
};
use crate::adlrt::custom::sys::types::maybe::Maybe;
use crate::parser::docstring_scoped_name;
use crate::processing::loader::loader_from_search_paths;
use crate::processing::resolver::Resolver;
use genco::fmt::{self, Indentation};
use genco::prelude::*;

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
    for mn in &opts.modules {
        if let Some(m) = resolver.get_module(mn) {
            // TODO sys.annotations::SerializedName needs to be embedded
            let mut tokens = js::Tokens::new();
            let mut mgen = TsGenVisitor {
                adlr: js::import("./runtime/adl", "ADL").into_wildcard(),
                _map: HashMap::new(),
                t: &mut tokens,
            };
            mgen.gen_module(m)?;
            let stdout = std::io::stdout();
            let mut w = fmt::IoWriter::new(stdout.lock());
            let fmt = fmt::Config::from_lang::<JavaScript>();
            let fmt = fmt::Config::with_indentation(fmt, Indentation::Space(2));

            let config = js::Config::default();
            // let config = js::Config{
            //     ..Default::default()
            // };
            tokens.format_file(&mut w.as_formatter(&fmt), &config)?;
        }
    }
    // let modules: Vec<&Module1> = resolver
    //     .get_module_names()
    //     .into_iter()
    //     .map(|mn| resolver.get_module(&mn).unwrap())
    //     .collect();
    // println!("{}", serde_json::to_string_pretty(&modules).unwrap());
    Ok(())
}

struct TsScopedDeclGenVisitor<'a> {
    module_name: &'a String,
    t: &'a mut Tokens<JavaScript>,
}

impl TsScopedDeclGenVisitor<'_> {
    fn lit(&mut self, s: &'static str) {
        self.t.append(Item::Literal(ItemStr::Static(s)));
    }
}

impl TsScopedDeclGenVisitor<'_> {
    fn visit_annotations(&mut self, d: &Annotations) {
        let mut keys: Vec<&ScopedName> = d.0.keys().collect();
        keys.sort_by(|a, b| {
            if &a.module_name == &b.module_name {
                if a.name == b.name {
                    Ordering::Equal
                } else if a.name > b.name {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            } else if a.module_name > b.module_name {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        });
        quote_in! { self.t =>  "annotations":$("[") };
        keys.iter().fold(false, |rest, key| {
            if **key == crate::parser::docstring_scoped_name() {
                return rest;
            }
            if rest {
                self.lit(",");
            }
            self.lit("{");
            let jv = &serde_json::to_string(d.0.get(key).unwrap()).unwrap();
            quote_in! { self.t => "value":$jv,}
            quote_in! { self.t => "key": }
            self.lit("{");
            quote_in! { self.t => "moduleName":$DQ$(&key.module_name)$DQ,"name":$DQ$(&key.name)$DQ }
            self.lit("}");
            self.lit("}");
            return true;
        });
        quote_in! { self.t =>  $("]") };
    }

    fn visit_decl(&mut self, d: &Decl<TypeExpr<TypeRef>>) {
        self.lit("{");
        self.visit_annotations(&d.annotations);
        self.lit(",");
        self.visit_decl_type(&d.r#type);
        self.lit(",");
        self.visit_decl_name(&d.name);
        self.lit(",");
        quote_in! { self.t => "version":{"kind":"nothing"}};
        self.lit("}");
    }
    fn visit_decl_name(&mut self, n: &String) {
        quote_in! { self.t =>  "name":$("\"")$n$("\"")};
    }
    fn visit_decl_type(&mut self, r#type: &DeclType<TypeExpr<TypeRef>>) {
        self.lit("\"type_\":{");
        match r#type {
            DeclType::Struct(dt) => self.visit_struct(dt),
            DeclType::Union(dt) => self.visit_union(dt),
            DeclType::Newtype(dt) => self.visit_newtype(dt),
            DeclType::Type(dt) => self.visit_typealias(dt),
        }
        self.lit("}");
    }
    fn visit_struct(&mut self, dt: &Struct<TypeExpr<TypeRef>>) {
        quote_in! { self.t => "kind":"struct_","value":$("{") }
        self.visit_type_params(&dt.type_params);
        self.lit(",");
        self.lit("\"fields\":[");
        dt.fields.iter().fold(false, |rest, f| {
            if rest {
                self.lit(",");
            }
            self.visit_field(f);
            return true;
        });
        self.lit("]");
        self.lit("}");
    }
    fn visit_union(&mut self, dt: &Union<TypeExpr<TypeRef>>) {
        quote_in! { self.t => "kind":"union_","value":$("{") }
        self.visit_type_params(&dt.type_params);
        self.lit(",");
        self.lit("\"fields\":[");
        dt.fields.iter().fold(false, |rest, f| {
            if rest {
                self.lit(",");
            }
            self.visit_field(f);
            return true;
        });
        self.lit("]");
        self.lit("}");
    }
    fn visit_newtype(&mut self, _dt: &NewType<TypeExpr<TypeRef>>) {
        // TODO
        println!("newtype: ");
    }
    fn visit_typealias(&mut self, _dt: &TypeDef<TypeExpr<TypeRef>>) {
        // TODO
        println!("type: ");
    }
    fn visit_type_params(&mut self, tps: &Vec<Ident>) {
        quote_in! { self.t => "typeParams":[$(for tp in tps join (,) => $DQ$tp$DQ)]}
    }
    fn visit_field(&mut self, f: &Field<TypeExpr<TypeRef>>) {
        self.lit("{");
        self.visit_annotations(&f.annotations);
        self.lit(",");
        quote_in! { self.t =>  "serializedName":$("\"")$(&f.serialized_name)$("\""), }
        self.visit_default(&f.default);
        self.lit(",");
        quote_in! { self.t =>  "name":$("\"")$(&f.name)$("\"")};
        self.lit(",");
        quote_in! { self.t =>  "typeExpr":}
        self.visit_type_expr(&f.type_expr);
        self.lit("}");
    }
    fn visit_default(&mut self, f: &Maybe<serde_json::Value>) {
        quote_in! { self.t =>  "default":$("{")};
        match f {
            Maybe(None) => {
                quote_in! { self.t =>  "kind":"nothing"}
            }
            Maybe(Some(v)) => {
                let jv = &serde_json::to_string(&v).unwrap();
                quote_in! { self.t =>  "kind":"just","value":$(jv)};
            }
        }
        quote_in! { self.t =>  $("}")};
    }
    fn visit_type_expr(&mut self, te: &TypeExpr<TypeRef>) {
        quote_in! { self.t =>  $("{")}
        self.visit_type_ref(&te.type_ref);
        quote_in! { self.t => ,"parameters":$("[")}
        te.parameters.iter().fold(false, |rest, p| {
            if rest {
                self.lit(",");
            }
            self.visit_type_expr(p);
            return true;
        });
        self.lit("]");
        self.lit("}");
    }
    fn visit_type_ref(&mut self, te: &TypeRef) {
        quote_in! { self.t => "typeRef":$("{")}
        match te {
            TypeRef::ScopedName(n) => {
                quote_in! { self.t =>  "kind":"reference","value":{"moduleName":$("\"")$(&n.module_name)$("\""),"name":$("\"")$(&n.name)$("\"")}};
            }
            TypeRef::LocalName(n) => {
                quote_in! { self.t =>  "kind":"reference","value":{"moduleName":$("\"")$(self.module_name)$("\""),"name":$("\"")$(n)$("\"")}};
            }
            TypeRef::Primitive(n) => {
                let p = crate::processing::primitives::str_from_prim(n.clone());
                // let p = &serde_json::to_string(n).unwrap();
                quote_in! { self.t =>  "kind":"primitive","value":$DQ$p$DQ};
            }
            TypeRef::TypeParam(n) => {
                quote_in! { self.t =>  "kind":"typeParam","value":$("\"")$n$("\"")};
            }
        }
        self.lit("}");
    }
}

struct TsGenVisitor<'a> {
    t: &'a mut Tokens<JavaScript>,
    adlr: JsImport,
    _map: HashMap<String, JsImport>,
}

impl TsGenVisitor<'_> {
    fn lit(&mut self, s: &'static str) {
        self.t.append(Item::Literal(ItemStr::Static(s)));
    }
}

struct RttiPayload<'a> {
    mname: String,
    type_params: &'a Vec<String>,
}

impl TsGenVisitor<'_> {
    fn gen_doc_comment(&mut self, annotations: &Annotations) {
        if let Some(ds) = annotations.0.get(&docstring_scoped_name()) {
            self.lit("/**\n");
            for c in ds.as_array().unwrap().iter() {
                if let Ok(x) = serde_json::to_string(&c.clone()) {
                    // TODO should this be trimmed? or should the output be "*$y" ie no space
                    let y = x[1..x.len() - 1].trim();
                    quote_in! {self.t => $[' ']* $(y)$['\r']};
                }
            }
            self.lit(" */\n");
        }
    }

    fn gen_rtti(
        &mut self,
        decl: &Decl<TypeExpr<TypeRef>>,
        payload: &RttiPayload,
    ) -> anyhow::Result<()> {
        // Generation AST holder
        let name = &decl.name;
        // let name_up = title(name);
        let mname = &payload.mname;
        quote_in! { self.t =>
            $['\n']
            const $(name)_AST : $(&self.adlr).ScopedDecl =
              {"moduleName":$("\"")$(mname.clone())$("\""),"decl":$(ref tok => {
                let mut sdg = TsScopedDeclGenVisitor{module_name: &mname.clone(), t: tok};
                sdg.visit_decl(decl);
              })};

            export const sn$(cap_or__(name)): $(&self.adlr).ScopedName = {moduleName:$("\"")$mname$("\""), name:$("\"")$name$("\"")};

            export function texpr$(cap_or__(name))<$(for tp in payload.type_params join (, ) => $tp)>($(ref t => texpr_args(t, &payload.type_params))): ADL.ATypeExpr<$(name)<$(for tp in payload.type_params join (, ) => $tp)>> {
                return {value:{typeRef:{kind:"reference",value:sn$(cap_or__(name))},parameters:[$(ref t => texpr_params(t, &payload.type_params))]}};
            }
            $['\n']
        }
        Ok(())
    }

    fn gen_module(&mut self, m: &Module<TypeExpr<TypeRef>>) -> anyhow::Result<()> {
        quote_in! { self.t =>
            $("/* @generated from adl module") $(m.name.clone()) $("*/")
            $['\n']
        };
        let mname = &m.name;
        for decl in m.decls.iter() {
            self.gen_doc_comment(&decl.annotations);
            let r = match &decl.r#type {
                DeclType::Struct(d) => self.gen_struct(
                    d,
                    DeclPayload {
                        decl: decl,
                        mname: &mname.clone(),
                    },
                ),
                DeclType::Union(d) => self.gen_union(
                    d,
                    DeclPayload {
                        decl: decl,
                        mname: &mname.clone(),
                    },
                ),
                DeclType::Newtype(d) => self.gen_newtype(d),
                DeclType::Type(d) => self.gen_type(d),
            };
            if let Err(_) = r {
                return r;
            }
        }
        self.lit("export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {\n");
        m.decls.iter().fold(false, |rest, decl| {
            if rest {
                self.lit(",\n")
            }
            self.lit("  ");
            quote_in! { self.t =>
                $("\"")$(m.name.clone()).$(&decl.name)$("\"") : $(&decl.name)_AST
            };
            true
        });
        self.lit("\n};");
        Ok(())
    }
}

const OC: &str = "{";
const CC: &str = "}";
const DQ: &str = "\"";
const SP: &str = " ";

// fn lit(t: &mut Tokens<JavaScript>, s: &'static str) {
//     t.append(Item::Literal(ItemStr::Static(s)));
// }

fn gen_type_params__<'a>(mut t: &mut Tokens<JavaScript>, used: &Vec<String>, type_params: &'a Vec<String>) {
    if type_params.len() > 0 {
        type_params.iter().fold(false, |rest, p| {
            if rest {
                quote_in! { t => ,$[' '] }
            }
            if !used.contains(p) {
                quote_in! { t => _ }
            }
            quote_in! { t => $p }
            true
        });
    }
}

fn texpr_args<'a>(mut t: &mut Tokens<JavaScript>, type_params: &'a Vec<String>) {
    type_params.iter().fold(false, |rest, p| {
        if rest {
            quote_in! { t => ,$[' '] };
        }
        quote_in! { t => texpr$p : ADL.ATypeExpr<$p> };
        true
    });
}

fn texpr_params<'a>(mut t: &mut Tokens<JavaScript>, type_params: &'a Vec<String>) {
    type_params.iter().fold(false, |rest, p| {
        if rest {
            quote_in! { t => , };
        }
        quote_in! { t => texpr$(p).value };
        true
    });
}

fn used_type_params(te_trs: &Vec<TypeExpr<TypeRef>>) -> Vec<String> {
    let fnames = &mut Vec::new();
    collect_used_type_params(&te_trs, fnames);
    fnames.to_vec()
}
fn collect_used_type_params(te_trs: &Vec<TypeExpr<TypeRef>>, mut fnames: &mut Vec<String>) {
    te_trs.iter().for_each(|te| {
        if let TypeRef::TypeParam(tp) = &te.type_ref {
            fnames.push(tp.clone());
        }
        collect_used_type_params(&te.parameters, &mut fnames);
    })
}

impl TsGenVisitor<'_> {
    fn gen_struct(
        &mut self,
        m: &Struct<TypeExpr<TypeRef>>,
        payload: DeclPayload,
    ) -> anyhow::Result<()> {
        let (decl, name) = (payload.decl, &payload.decl.name);
        // let name_up = &title(name);
        self.gen_doc_comment(&decl.annotations);
        // let fnames: &Vec<String> = &m.fields.iter().map(|f| f.name.clone()).collect();
        let te_trs: Vec<TypeExpr<TypeRef>> = m.fields.iter().map(|f| f.type_expr.clone()).collect();
        let fnames = used_type_params(&te_trs);
        quote_in! { self.t =>
            export interface $(name)<$(ref t => gen_type_params__(t, &fnames, &m.type_params))> $OC$['\r']
        }
        let mut has_make = true;
        for f in m.fields.iter() {
            self.gen_doc_comment(&f.annotations);
            let rt = rust_type(&f.type_expr).map_err(|s| anyhow!(s))?;
            has_make = has_make && rt.0;
            quote_in! { self.t =>
                $SP$SP$(&f.name): $(rt.1);$['\r']
            }
        }
        quote_in! { self.t =>
            $CC$['\r']$['\n']
        }
        if has_make {
            quote_in! { self.t =>
                export function make$(cap_or__(name))<$(ref t => gen_type_params__(t, &fnames, &m.type_params))>(
                  $(if m.fields.len() == 0 => _)input: {
                    $(ref tok => struct_field_make_input(tok, &m.fields)?)
                  }
                ): $(name)<$(ref t => gen_type_params__(t, &fnames, &m.type_params))> {
                  return {
                    $(ref tok => struct_field_make_return(tok, &m.fields))
                  };
                }
            }
        }
        self.gen_rtti(
            decl,
            &RttiPayload {
                mname: payload.mname.clone(),
                type_params: &m.type_params,
            },
        )?;
        Ok(())
    }
}

fn struct_field_make_input(
    t: &mut Tokens<JavaScript>,
    fs: &Vec<Field<TypeExpr<TypeRef>>>,
) -> anyhow::Result<()> {
    for f in fs {
        let rt = rust_type(&f.type_expr).map_err(|s| anyhow!(s))?;
        quote_in! { *t =>
          $(&f.name): $(rt.1),$['\r']
        }
    }
    Ok(())
}

fn struct_field_make_return(t: &mut Tokens<JavaScript>, fs: &Vec<Field<TypeExpr<TypeRef>>>) {
    for f in fs {
        quote_in! { *t =>
          $(&f.name): input.$(&f.name),$['\r']
        }
    }
}

// struct DeclPayload<'a>(&'a Decl<TypeExpr<TypeRef>>);
struct DeclPayload<'a> {
    decl: &'a Decl<TypeExpr<TypeRef>>,
    mname: &'a String,
}

impl TsGenVisitor<'_> {
    fn gen_union(
        &mut self,
        m: &Union<TypeExpr<TypeRef>>,
        payload: DeclPayload,
    ) -> anyhow::Result<()> {
        let name = &payload.decl.name;
        // self.lit("// union \n");
        let is_enum = m
            .fields
            .iter()
            .find(|f| match &f.type_expr.type_ref {
                TypeRef::Primitive(p) => match p {
                    PrimitiveType::Void => false,
                    _ => true,
                },
                _ => true,
            })
            .is_none();
        if !is_enum {
            // let bnames_up = m.fields.iter().map(|b| title(&b.name));
            let mut opts = vec![];
            for b in m.fields.iter() {
                self.gen_doc_comment(&b.annotations);
                let bname = b.name.clone();
                // let bname_up = title(&b.name);
                let rtype = rust_type(&b.type_expr).map_err(|s| anyhow!(s))?;
                // &vec![rtype.1.clone()]
                let used = used_type_params(&vec![b.type_expr.clone()]);

                opts.push((bname.clone(), rtype.clone().1));
                quote_in! { self.t =>
                    export interface $(name)_$(bname.clone())<$(ref t => gen_type_params__(t, &used, &m.type_params))> {
                        kind: $("'")$(bname.clone())$("'");
                        value: $(rtype.1.clone());
                    }$['\r']
                }
            }
            let te_trs: Vec<TypeExpr<TypeRef>> = m.fields.iter().map(|f| f.type_expr.clone()).collect();
            let tp_names = used_type_params(&te_trs);
            quote_in! { self.t =>
                $['\n']
                export type $name<$(ref t => gen_type_params__(t, &tp_names, &m.type_params))> = $(for n in m.fields.iter().map(|b| &b.name) join ( | ) => $(name)_$n<$(ref t => gen_type_params__(t, &tp_names, &m.type_params))>);

                export interface $(name)Opts<$(ref t => gen_type_params__(t, &tp_names, &m.type_params))> {
                  $(for opt in opts => $(opt.0): $(opt.1);$['\r'])
                }$['\n']

                export function make$(name)<$(ref t => gen_type_params__(t, &tp_names, &m.type_params))$(if m.type_params.len() > 0 => ,$[' '] )K extends keyof $(name)Opts<$(ref t => gen_type_params__(t, &tp_names, &m.type_params))>>(kind: K, value: $(name)Opts<$(ref t => gen_type_params__(t, &tp_names, &m.type_params))>[K]) { return {kind, value}; }$['\n']
            }
        } else {
            let b_names: Vec<&String> = m.fields.iter().map(|f| &f.name).collect();
            let b_len = b_names.len();
            let b1 = if b_len > 0 { b_names[0] } else { "" };
            quote_in! { self.t =>
                $['\n']
                export type $name = $(for n in b_names join ( | ) => $("'")$(n)$("'"));
                $['\r']
            }
            // TODO not sure what this is for -- duplicating existing ts
            if b_len == 1 {
                quote_in! { self.t => export const values$name : $name[] = [$("'")$(b1)$("'")];$['\r'] }
            }
        }
        self.gen_rtti(
            payload.decl,
            &RttiPayload {
                mname: payload.mname.clone(),
                type_params: &m.type_params,
            },
        )?;
        Ok(())
    }

    fn gen_newtype(&mut self, _m: &NewType<TypeExpr<TypeRef>>) -> anyhow::Result<()> {
        quote_in! { self.t =>
            $("// newtype")
        }
        Ok(())
    }

    fn gen_type(&mut self, _m: &TypeDef<TypeExpr<TypeRef>>) -> anyhow::Result<()> {
        quote_in! { self.t =>
            $("// type")
        }
        Ok(())
    }
}

pub fn cap_or__(input: &String) -> String {
    let mut c = input.chars();
    match c.next() {
        None => String::new(),
        Some(first) => {
            if first.is_uppercase() {
                return input.clone();
            } else {
                return "_".to_string() + input;
            }
        },
    }
}

pub fn to_title(input: &String) -> String {
    let mut c = input.chars();
    match c.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().to_string() + &String::from(&input[1..]),
    }
}

/// returns (has_make_function,ts type)
fn rust_type(te: &TypeExpr<TypeRef>) -> Result<(bool, String), String> {
    match &te.type_ref {
        TypeRef::ScopedName(_n) => todo!(),
        TypeRef::LocalName(n) => tstype_from_local_name(n, &te.parameters),
        TypeRef::Primitive(n) => tstype_from_prim(n, &te.parameters),
        TypeRef::TypeParam(n) => Ok((true, n.clone())),
    }
}

fn tstype_from_local_name(
    local_name: &String,
    params: &Vec<TypeExpr<TypeRef>>,
) -> Result<(bool, String), String> {
    if params.len() > 0 {
        let mut tperr = vec![];
        let tps: Vec<String> = params
            .iter()
            .filter_map(|p| {
                let r = rust_type(p);
                match r {
                    Ok((_, t)) => Some(t),
                    Err(e) => {
                        tperr.push(e);
                        return None;
                    }
                }
            })
            .collect();
        if tperr.len() != 0 {
            let msg = tperr.join("\n\t");
            return Err(format!("Error constructing type param: {}", msg));
        }
        let tpstr = format!("{}<{}>", local_name.clone(), tps.join(","));
        return Ok((true, tpstr));
    }
    Ok((true, local_name.clone()))
}

fn tstype_from_prim(
    prim: &PrimitiveType,
    params: &Vec<TypeExpr<TypeRef>>,
) -> Result<(bool, String), String> {
    match prim {
        PrimitiveType::Void => Ok((true, "null".to_string())),
        PrimitiveType::Bool => Ok((true, "boolean".to_string())),
        PrimitiveType::Int8 => Ok((true, "number".to_string())),
        PrimitiveType::Int16 => Ok((true, "number".to_string())),
        PrimitiveType::Int32 => Ok((true, "number".to_string())),
        PrimitiveType::Int64 => Ok((true, "number".to_string())),
        PrimitiveType::Word8 => Ok((true, "number".to_string())),
        PrimitiveType::Word16 => Ok((true, "number".to_string())),
        PrimitiveType::Word32 => Ok((true, "number".to_string())),
        PrimitiveType::Word64 => Ok((true, "number".to_string())),
        PrimitiveType::Float => Ok((true, "number".to_string())),
        PrimitiveType::Double => Ok((true, "number".to_string())),
        PrimitiveType::Json => Ok((true, "{}|null".to_string())),
        PrimitiveType::ByteVector => Ok((true, "Uint8Array".to_string())),
        PrimitiveType::String => Ok((true, "string".to_string())),
        _ => {
            if params.len() != 1 {
                return Err(format!( "Primitive parameterized type require 1 and only one param. Type {:?} provided with {}", prim, params.len() ));
            }
            let param_type = rust_type(&params[0])?;
            match prim {
                PrimitiveType::Vector => {
                    return Ok((param_type.0, format!("{}[]", param_type.1)));
                }
                PrimitiveType::StringMap => Ok((
                    param_type.0,
                    format!("{}[key: string]: {}{}", "{", param_type.1, "}"),
                )),
                PrimitiveType::Nullable => Ok((param_type.0, format!("({}|null)", param_type.1))),
                PrimitiveType::TypeToken => Ok((false, format!("ADL.ATypeExpr<{}>", param_type.1))),
                _ => Err(format!("unknown primitive {:?}", prim)),
            }
        }
    }
}
