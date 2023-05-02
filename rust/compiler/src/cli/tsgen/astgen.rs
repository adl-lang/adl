use std::cmp::Ordering;
use genco::tokens::{Item, ItemStr};

use crate::adlgen::sys::adlast2::{
    Annotations, Decl, DeclType, Field, Ident, NewType, ScopedName, Struct,
    TypeDef, TypeExpr, TypeRef, Union,
};
use crate::adlrt::custom::sys::types::maybe::Maybe;
use genco::prelude::*;

const DQ: &str = "\"";

pub struct TsScopedDeclGenVisitor<'a> {
    pub module_name: &'a String,
    pub t: &'a mut Tokens<JavaScript>,
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

    pub fn visit_decl(&mut self, d: &Decl<TypeExpr<TypeRef>>) {
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
    fn visit_newtype(&mut self, dt: &NewType<TypeExpr<TypeRef>>) {
        quote_in! { self.t => "kind":"newtype_","value":$("{") }
        self.visit_type_params(&dt.type_params);
        self.lit(",");
        self.visit_default(&dt.default);
        self.lit(",");
        quote_in! { self.t =>  "typeExpr":}
        self.visit_type_expr(&dt.type_expr);
        self.lit("}");
    }
    fn visit_typealias(&mut self, dt: &TypeDef<TypeExpr<TypeRef>>) {
        quote_in! { self.t => "kind":"type_","value":$("{") }
        self.visit_type_params(&dt.type_params);
        self.lit(",");
        quote_in! { self.t =>  "typeExpr":}
        self.visit_type_expr(&dt.type_expr);
        self.lit("}");
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