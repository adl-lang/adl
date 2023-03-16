use super::TsOpts;

use std::collections::HashMap;

use anyhow::anyhow;
use genco::prelude::js::Import;
use genco::tokens::{Item, ItemStr};

use crate::adlgen::sys::adlast2::{
    Decl, DeclType, Module, NewType, ScopedDecl, Struct, TypeDef, TypeExpr, TypeRef, Union,
};
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
        // TODO why is the resolver by the filename not the module name
        if let Some(m) = resolver.get_module(mn) {
            gen_module(m);
            // println!("// gen {}", m.name);
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

struct Imports {
    adlr: Import,
    map: HashMap<String, Import>,
}

fn gen_module(m: &Module<TypeExpr<TypeRef>>) -> anyhow::Result<()> {
    let mut tokens = js::Tokens::new();
    let imports = Imports {
        adlr: js::import("./runtime/adl", "ADL").into_wildcard(),
        map: HashMap::new(),
    };
    // let adlr = &js::import("./runtime/adl", "ADL").into_wildcard();

    // println!("// gen {}", m.name);

    quote_in! { tokens =>
        $("/* @generated from adl module") $(m.name.clone()) $("*/")
        $['\n']
    };

    for decl in m.decls.iter() {
        // let scopedDecl = ScopedDecl::new(m.name.clone(), decl);
        // let scopedDecl = ScopedDecl {
        //     module_name: m.name.clone(),
        //     decl: *decl,
        // };
        let r = match &decl.r#type {
            DeclType::Struct(d) => Ok(()),
            DeclType::Union(d) => gen_union(&mut tokens, &imports, m.name.clone(), decl, d),
            DeclType::Newtype(d) => Ok(()),
            DeclType::Type(d) => Ok(()),
        };
        if let Err(_) = r {
            return r;
        }
    }

    tokens.append(Item::Literal(ItemStr::Static(
        "export const _AST_MAP: { [key: string]: ADL.ScopedDecl } = {\n",
    )));
    for decl in m.decls.iter() {
        quote_in! { tokens =>
            $[' ']$[' ']$("\"")$(m.name.clone()).$(&decl.name)$("\"") : $(&decl.name)_AST,$['\r']
        }
    }
    tokens.append(Item::Literal(ItemStr::Static("}")));

    let stdout = std::io::stdout();
    let mut w = fmt::IoWriter::new(stdout.lock());

    let fmt = fmt::Config::from_lang::<JavaScript>();
    let config = js::Config::default();

    tokens.format_file(&mut w.as_formatter(&fmt), &config)?;
    Ok(())
}

fn gen_struct(
    tokens: &mut Tokens<JavaScript>,
    imports: &Imports,
    name: &String,
    m: &Struct<TypeExpr<TypeRef>>,
) -> anyhow::Result<()> {
    quote_in! { *tokens =>
        $("// struct")
        // export type $name;

        // const $(name)_AST : $(&imports.adlr).ScopedDecl
        // export const sn$(name): $(&imports.adlr).ScopedName = {moduleName:"test5", name:"U1"};
        $['\n']
    }
    Ok(())
}

fn rust_type<TE>(te: &TypeExpr<TE>) -> String {
    return "number".to_string();
}

fn gen_union(
    tokens: &mut Tokens<JavaScript>,
    imports: &Imports,
    mname: String,
    decl: &Decl<TypeExpr<TypeRef>>,
    m: &Union<TypeExpr<TypeRef>>,
) -> anyhow::Result<()> {
    // let scopedDecl = ScopedDecl::new(mname.clone(), *decl);
    // let scopedDecl = ScopedDecl {
    //     module_name: mname.clone(),
    //     decl: *(decl.clone()),
    // };
    // TODO this is wireformat need TS format 
    // TODO sys.annotations::SerializedName needs to be embedded
    let astDecl = serde_json::to_string(decl).unwrap();
    let name = &decl.name;
    tokens.append(Item::Literal(ItemStr::Static("// union \n")));
    // let ast = serde_json::to_string(&scopedDecl).unwrap();
    let mut bNames = vec![];
    let mut opts = vec![];
    for b in m.fields.iter() {
        let bname = b.name.clone();
        let bName = b.name.to_uppercase();
        bNames.push(bName.clone());
        let rtype = rust_type(&b.type_expr);
        opts.push((bname.clone(), rtype.clone()));
        quote_in! { *tokens =>
            export interface $(name)_$(bName) {
                kind: $("'")$(bname)$("'");
                value: $(rtype);
            }$['\r']
        }
    }
    quote_in! { *tokens =>
        $['\n']
        export type $name = $(for n in bNames join ( | ) => $(name)_$n);

        export interface $(name)Opts {
          $(for opt in opts => $(opt.0): $(opt.1);$['\r'])
        }$['\n']

        export function make$(name)<K extends keyof $(name)Opts>(kind: K, value: $(name)Opts[K]) { return {kind, value}; }

        const $(name)_AST : $(&imports.adlr).ScopedDecl =
          { "moduleName": $("\"")$(mname.clone())$("\""), $("\"")decl$("\""): $astDecl}

        export const sn$(name): $(&imports.adlr).ScopedName = {moduleName:$("\"")$mname$("\""), name:$("\"")$name$("\"")};

        export function texpr$(name)(): ADL.ATypeExpr<$(name)> {
            return {value : {typeRef : {kind: "reference", value : sn$(name)}, parameters : []}};
        }
        $['\n']
    }
    Ok(())
}

fn gen_newtype(
    tokens: &mut Tokens<JavaScript>,
    imports: &Imports,
    name: &String,
    m: &NewType<TypeExpr<TypeRef>>,
) -> anyhow::Result<()> {
    quote_in! { *tokens =>
        $("// newtype")
    }
    Ok(())
}

fn gen_type(
    tokens: &mut Tokens<JavaScript>,
    imports: &Imports,
    name: &String,
    m: &TypeDef<TypeExpr<TypeRef>>,
) -> anyhow::Result<()> {
    quote_in! { *tokens =>
        $("// type")
    }
    Ok(())
}
