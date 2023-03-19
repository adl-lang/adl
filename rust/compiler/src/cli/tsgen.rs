use super::TsOpts;

use std::collections::HashMap;
use std::error::Error;
use std::str::Chars;

use anyhow::anyhow;
use genco::prelude::js::Import as JsImport;
use genco::tokens::{Item, ItemStr};

use crate::adlgen::sys::adlast2::{
    Annotation, Decl, DeclType, Field, Ident, Import, Module, NewType, PrimitiveType, ScopedName,
    Struct, TypeDef, TypeExpr, TypeRef, Union,
};
use crate::adlrt::custom::sys::types::map::Map;
use crate::adlrt::custom::sys::types::maybe::Maybe;
use crate::parser::docstring_scoped_name;
use crate::processing::loader::loader_from_search_paths;
use crate::processing::resolver::Resolver;
use genco::fmt;
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
                map: HashMap::new(),
                toks: &mut tokens,
            };
            mgen.gen_module(m);

            let stdout = std::io::stdout();
            let mut w = fmt::IoWriter::new(stdout.lock());
            let fmt = fmt::Config::from_lang::<JavaScript>();
            let config = js::Config::default();
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

trait Visitor {
    fn visit_Module(&mut self, m: &Module<TypeExpr<TypeRef>>);
    fn visit_annotations(&mut self, d: &Map<ScopedName, serde_json::Value>);
    fn visit_module_name(&mut self, n: &String);
    fn visit_import(&mut self, i: &Import);
    fn visit_decl(&mut self, d: &Decl<TypeExpr<TypeRef>>);
    fn visit_decl_name(&mut self, n: &String);
    fn visit_decl_type(&mut self, r#type: &DeclType<TypeExpr<TypeRef>>);
    fn visit_struct(&mut self, dt: &Struct<TypeExpr<TypeRef>>);
    fn visit_union(&mut self, dt: &Union<TypeExpr<TypeRef>>);
    fn visit_newtype(&mut self, dt: &NewType<TypeExpr<TypeRef>>);
    fn visit_typealias(&mut self, dt: &TypeDef<TypeExpr<TypeRef>>);
    fn visit_type_params(&mut self, tps: &Vec<Ident>);
    fn visit_field(&mut self, f: &Field<TypeExpr<TypeRef>>);
    fn visit_default(&mut self, f: &Maybe<serde_json::Value>);
    fn visit_type_expr(&mut self, te: &TypeExpr<TypeRef>);
    fn visit_type_ref(&mut self, te: &TypeRef);
}

struct TsScopedDeclGenVisitor<'a> {
    module_name: &'a String,
    toks: &'a mut Tokens<JavaScript>,
}

impl TsScopedDeclGenVisitor<'_> {
    fn lit(&mut self, s: &'static str) {
        self.toks.append(Item::Literal(ItemStr::Static(s)));
    }
}

impl TsScopedDeclGenVisitor<'_> {
    fn visit_annotations(&mut self, d: &Vec<Annotation>) {
        quote_in! { self.toks =>  "annotations":$("[") };
        d.iter().fold(false, |rest, a| {
            if a.key == crate::parser::docstring_scoped_name() {
                return rest;
            }
            if rest {
                self.lit(",");
            }
            self.lit("{");
            let jv = &serde_json::to_string(&a.value).unwrap();
            quote_in! { self.toks => "value":$jv,}
            quote_in! { self.toks => "key": }
            self.lit("{");
            quote_in! { self.toks => "moduleName":$("\"")$(&a.key.module_name)$("\""),"name": $("\"")$(&a.key.name)$("\"") }
            self.lit("}");
            self.lit("}");
            return true;
        });
        quote_in! { self.toks =>  $("]") };
    }

    fn visit_decl(&mut self, d: &Decl<TypeExpr<TypeRef>>) {
        self.lit("{");
        self.visit_annotations(&d.annotations);
        self.lit(",");
        self.visit_decl_type(&d.r#type);
        self.lit(",");
        self.visit_decl_name(&d.name);
        self.lit(",");
        quote_in! { self.toks => "version":{"kind":"nothing"}};
        self.lit("}");
    }
    fn visit_decl_name(&mut self, n: &String) {
        quote_in! { self.toks =>  "name":$("\"")$n$("\"")};
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
        quote_in! { self.toks => "kind":"struct_","value":$("{") }
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
        quote_in! { self.toks => "kind":"union_","value":$("{") }
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
    fn visit_newtype(&mut self, dt: &NewType<TypeExpr<TypeRef>>) {
        // TODO
        println!("newtype: ");
    }
    fn visit_typealias(&mut self, dt: &TypeDef<TypeExpr<TypeRef>>) {
        // TODO
        println!("type: ");
    }
    fn visit_type_params(&mut self, tps: &Vec<Ident>) {
        quote_in! { self.toks => "typeParams":[$(for tp in tps join (,) => $tp)]}
    }
    fn visit_field(&mut self, f: &Field<TypeExpr<TypeRef>>) {
        self.lit("{");
        self.visit_annotations(&f.annotations);
        self.lit(",");
        quote_in! { self.toks =>  "serializedName":$("\"")$(&f.serialized_name)$("\""), }
        self.visit_default(&f.default);
        self.lit(",");
        self.visit_type_expr(&f.type_expr);
        self.lit(",");
        quote_in! { self.toks =>  "name":$("\"")$(&f.name)$("\"")};
        self.lit("}");
    }
    fn visit_default(&mut self, f: &Maybe<serde_json::Value>) {
        quote_in! { self.toks =>  "default":$("{")};
        match f {
            Maybe(None) => {
                quote_in! { self.toks =>  "kind":"nothing"}
            }
            Maybe(Some(v)) => {
                let jv = &serde_json::to_string(&v).unwrap();
                quote_in! { self.toks =>  "kind":"just","value":$(jv)};
            }
        }
        quote_in! { self.toks =>  $("}")};
    }
    fn visit_type_expr(&mut self, te: &TypeExpr<TypeRef>) {
        quote_in! { self.toks =>  "typeExpr":$("{")}
        self.visit_type_ref(&te.type_ref);
        quote_in! { self.toks => ,"parameters":$("[")}
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
        quote_in! { self.toks => "typeExpr":$("{")}
        match te {
            TypeRef::ScopedName(n) => {
                quote_in! { self.toks =>  "kind":"reference","value":{"moduleName":$("\""):$(&n.module_name)$("\""),"name":$("\"")$(&n.name)$("\"")}};
            }
            TypeRef::LocalName(n) => {
                quote_in! { self.toks =>  "kind":"reference","value":{"moduleName":$("\""):$(self.module_name)$("\""),"name":$("\"")$(n)$("\"")}};
            }
            TypeRef::Primitive(n) => {
                let p = &serde_json::to_string(n).unwrap();
                quote_in! { self.toks =>  "kind":"primitive","value":$p};
            }
            TypeRef::TypeParam(n) => {
                quote_in! { self.toks =>  "kind":"typeParam","value":$("\"")$n$("\"")};
            }
        }
        self.lit("}");
    }
}

struct TsGenVisitor<'a> {
    toks: &'a mut Tokens<JavaScript>,
    adlr: JsImport,
    map: HashMap<String, JsImport>,
}

// struct TsComment<'a> {
//     toks: &'a mut Tokens<JavaScript>,
// }

// impl std::io::Write for TsComment<'_> {
//     fn write(&mut self, buf: &[u8]) -> Result<usize, std::io::Error> {
//         // buf.to
//         // let string = String::from_utf8_lossy(buf).;
//         // let string = unsafe {
//         //     // We do not emit invalid UTF-8.
//         //     String::from
//         //     String::from_utf8_unchecked(buf)
//         // };
//         // quote_in! { self.toks => $(string)}
//         return Ok(buf.len());
//     }

//     fn flush(&mut self) -> std::io::Result<()> {
//         todo!()
//     }
// }

impl TsGenVisitor<'_> {
    fn lit(&mut self, s: &'static str) {
        self.toks.append(Item::Literal(ItemStr::Static(s)));
    }
    // fn to_comment(&mut self, val: &serde_json::Value) {
    //     serde_json::to_writer(TsComment { toks: self.toks }, val);
    // }
}

impl TsGenVisitor<'_> {
    fn gen_doc_comment(&mut self, value: &serde_json::Value) -> anyhow::Result<()> {
        self.lit("/**\n");
        for c in value.as_array().unwrap().iter() {
            if let Ok(x) = serde_json::to_string(&c.clone()) {
                let y = x[1..x.len() - 1].trim();
                quote_in! {self.toks => $[' ']* $(y)$['\r']};
            }
        }
        self.lit(" */\n");
        Ok(())
    }
    fn gen_module(&mut self, m: &Module<TypeExpr<TypeRef>>) -> anyhow::Result<()> {
        quote_in! { self.toks =>
            $("/* @generated from adl module") $(m.name.clone()) $("*/")
            $['\n']
        };
        for decl in m.decls.iter() {
            if let Some(ds) = decl
                .annotations
                .iter()
                .find(|a| a.key == docstring_scoped_name())
            {
                self.gen_doc_comment(&ds.value);
            }
            let r = match &decl.r#type {
                DeclType::Struct(d) => self.gen_struct(d),
                DeclType::Union(d) => self.gen_union(d, GenUnionPayload(decl)),
                DeclType::Newtype(d) => self.gen_newtype(d),
                DeclType::Type(d) => self.gen_type(d),
            };
            // Generation AST holder
            let name = &decl.name;
            let mname = m.name.clone();
            quote_in! { self.toks =>
                $['\n']
                const $(name)_AST : $(&self.adlr).ScopedDecl =
                  {"moduleName":$("\"")$(mname.clone())$("\""),"decl":$(ref tok => {
                    let mut sdg = TsScopedDeclGenVisitor{module_name: &mname.clone(), toks: tok};
                    sdg.visit_decl(decl);
                  })}

                export const sn$(name): $(&self.adlr).ScopedName = {moduleName:$("\"")$mname$("\""), name:$("\"")$name$("\"")};

                export function texpr$(name)(): ADL.ATypeExpr<$(name)> {
                    return {value : {typeRef : {kind: "reference", value : sn$(name)}, parameters : []}};
                }
                $['\n']
            }

            if let Err(_) = r {
                return r;
            }
        }
        self.lit("export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {\n");
        for decl in m.decls.iter() {
            quote_in! { self.toks =>
                $[' ']$[' ']$("\"")$(m.name.clone()).$(&decl.name)$("\"") : $(&decl.name)_AST,$['\r']
            }
        }
        self.lit("}");
        Ok(())
    }
}

impl TsGenVisitor<'_> {
    fn gen_struct(&mut self, m: &Struct<TypeExpr<TypeRef>>) -> anyhow::Result<()> {
        quote_in! { self.toks =>
            $("// struct")
            // export type $name;

            // const $(name)_AST : $(&imports.adlr).ScopedDecl
            // export const sn$(name): $(&imports.adlr).ScopedName = {moduleName:"test5", name:"U1"};
            $['\n']
        }
        Ok(())
    }
}

pub fn capitalize_first(input: &String) -> String {
    let mut c = input.chars();
    match c.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().to_string() + &String::from(&input[1..]),
    }
}
struct GenUnionPayload<'a>(&'a Decl<TypeExpr<TypeRef>>);

impl TsGenVisitor<'_> {
    fn gen_union(
        &mut self,
        m: &Union<TypeExpr<TypeRef>>,
        payload: GenUnionPayload,
    ) -> anyhow::Result<()> {
        let (name) = (&payload.0.name);
        self.lit("// union \n");
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
            let mut bnames_up = vec![];
            let mut opts = vec![];
            for b in m.fields.iter() {
                if let Some(ds) = b
                    .annotations
                    .iter()
                    .find(|a| a.key == docstring_scoped_name())
                {
                    self.gen_doc_comment(&ds.value);
                }
                let bname = b.name.clone();
                let bname_up = capitalize_first(&b.name);
                bnames_up.push(bname_up.clone());
                let rtype = rust_type(&b.type_expr);
                opts.push((bname.clone(), rtype.clone()));
                quote_in! { self.toks =>
                    export interface $(name)_$(bname_up) {
                        kind: $("'")$(bname)$("'");
                        value: $(rtype);
                    }$['\r']
                }
            }
            quote_in! { self.toks =>
                $['\n']
                export type $name = $(for n in bnames_up join ( | ) => $(name)_$n);

                export interface $(name)Opts {
                  $(for opt in opts => $(opt.0): $(opt.1);$['\r'])
                }$['\n']

                export function make$(name)<K extends keyof $(name)Opts>(kind: K, value: $(name)Opts[K]) { return {kind, value}; }$['\n']
            }
        } else {
            let b_names: Vec<&String> = m.fields.iter().map(|f| &f.name).collect();
            let b_len = b_names.len();
            let b1 = if b_len > 0 { b_names[0] } else { "" };
            quote_in! { self.toks =>
                $['\n']
                export type $name = $(for n in b_names join ( | ) => $("'")$(n)$("'"));
                $['\r']
            }
            // TODO not sure what this is for -- duplicating existing ts
            if b_len == 1 {
                quote_in! { self.toks => export const values$name : $name[] = [$("'")$(b1)$("'")];$['\r'] }
            }
        }
        Ok(())
    }

    fn gen_newtype(&mut self, m: &NewType<TypeExpr<TypeRef>>) -> anyhow::Result<()> {
        quote_in! { self.toks =>
            $("// newtype")
        }
        Ok(())
    }

    fn gen_type(&mut self, m: &TypeDef<TypeExpr<TypeRef>>) -> anyhow::Result<()> {
        quote_in! { self.toks =>
            $("// type")
        }
        Ok(())
    }
}

fn rust_type<TE>(te: &TypeExpr<TE>) -> String {
    return "number".to_string();
}
