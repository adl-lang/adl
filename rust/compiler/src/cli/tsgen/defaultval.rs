use std::collections::HashMap;

use anyhow::anyhow;
use serde_json::Value;

use crate::adlgen::sys::adlast2::{
    Decl, DeclType, Field, Module, PrimitiveType, ScopedName, TypeExpr, TypeRef, Union,
};
use crate::processing::resolver::Resolver;
use genco::prelude::*;

const DQ: &str = "\"";
const OC: &str = "{";
const CC: &str = "}";
const OSB: &str = "[";
const CSB: &str = "]";

pub struct ResolverModule<'a> {
    pub module: &'a Module<TypeExpr<TypeRef>>,
    pub resolver: &'a Resolver,
}

pub struct TsDefaultValue<'a> {
    pub ctx: &'a ResolverModule<'a>,
    pub decl: &'a Decl<TypeExpr<TypeRef>>,
    pub type_map: &'a HashMap<String, &'a TypeExpr<TypeRef>>,
    // pub depth: Box<i64>,
}

impl TsDefaultValue<'_> {
    fn create_wrapped_err(&self, f_name: &String, msg: String) -> anyhow::Result<()> {
        return Err(anyhow!(
            "Error in {}.{}::{}.\n\t{}",
            self.ctx.module.name,
            self.decl.name,
            f_name,
            msg,
        ));
    }

    fn create_err_union(&self, f_name: &String, val: &Value) -> anyhow::Result<()> {
        let x = serde_json::to_string(val).unwrap();
        // todo!();
        return Err(anyhow!(
            "Union expected a JSON object with one elememnt. For {}.{}::{} received '{}'",
            self.ctx.module.name,
            self.decl.name,
            f_name,
            x
        ));
    }

    fn create_err_union_te(&self, b_name: &String) -> anyhow::Result<()> {
        return Err(anyhow!(
            "Union branch with serialized_name not found. For {}.{} branch name '{}'",
            self.ctx.module.name,
            self.decl.name,
            b_name
        ));
    }

    fn create_err_union_void_branch(&self, b_name: &String) -> anyhow::Result<()> {
        return Err(anyhow!(
            "Union branch type Void expected. For {}.{} branch name '{}'",
            self.ctx.module.name,
            self.decl.name,
            b_name
        ));
    }

    fn create_err(&self, typename: &str, f_name: &String, val: &Value) -> anyhow::Result<()> {
        let x = serde_json::to_string(val).unwrap();
        // todo!();
        return Err(anyhow!(
            "default value does not match. Expected '{}' for {}.{}::{} received '{}'",
            typename,
            self.ctx.module.name,
            self.decl.name,
            f_name,
            x
        ));
    }

    fn create_err_msg(
        &self,
        typename: &str,
        f_name: &String,
        val: &Value,
        msg: String,
    ) -> anyhow::Result<()> {
        let x = serde_json::to_string(val).unwrap();
        return Err(anyhow!(
            "default value does not match. Expected '{}' for {}.{}::{} received '{}'\n{}",
            typename,
            self.ctx.module.name,
            self.decl.name,
            f_name,
            x,
            msg,
        ));
    }

    fn create_err_mismatch_type_params(
        &self,
        decl_name: &String,
        got: usize,
        expected: usize,
    ) -> anyhow::Result<()> {
        return Err(anyhow!(
            "Mismatched number of type parameters. Type constructur for '{}.{}' expected {} arguments, but was passed {}",
            self.ctx.module.name,
            decl_name,
            got,
            expected,
        ));
    }

    fn create_err_missing_val(&self, f_name: &String) -> anyhow::Result<()> {
        return Err(anyhow!(
            "Missing value or default value. {}.{}::{}",
            self.ctx.module.name,
            self.decl.name,
            f_name,
        ));
    }

    fn create_err_missing_type_param(&self, f_name: &String, tp: &String) -> anyhow::Result<()> {
        return Err(anyhow!(
            "Missing type param. {}.{}::{}<{}>",
            self.ctx.module.name,
            self.decl.name,
            f_name,
            tp,
        ));
    }

    fn create_err_resolve_scoped_name(
        &self,
        f_name: &String,
        sn: &ScopedName,
    ) -> anyhow::Result<()> {
        return Err(anyhow!(
            "Cannot resolve scoped name {}.{}. {}.{}::{}",
            sn.module_name,
            sn.name,
            self.ctx.module.name,
            self.decl.name,
            f_name,
        ));
    }

    fn create_err_resolve_module(
        &self,
        f_name: &String,
        module_name: &String,
    ) -> anyhow::Result<()> {
        return Err(anyhow!(
            "Cannot resolve module name {}. From {}.{}::{}",
            module_name,
            self.ctx.module.name,
            self.decl.name,
            f_name,
        ));
    }
}

impl TsDefaultValue<'_> {
    // fn find_local_decl(&self, name: &String) -> &Decl<TypeExpr<TypeRef>> {
    //     self.ctx
    //         .module
    //         .decls
    //         .iter()
    //         .find(|decl| decl.name == *name)
    //         .unwrap()
    // }

    pub fn gen_default_value(
        &self,
        t: &mut Tokens<JavaScript>,
        field: &Field<TypeExpr<TypeRef>>,
        val: Option<&Value>,
    ) -> anyhow::Result<()> {
        let val1 = match val {
            Some(v) => v,
            None => match &field.default.0 {
                Some(v) => v,
                None => {
                    // todo!();
                    return self.create_err_missing_val(&field.name);
                }
            },
        };
        self.gen_type_expr(t, &field.name, &field.type_expr, val1)?;
        Ok(())
    }

    pub fn gen_type_expr(
        &self,
        t: &mut Tokens<JavaScript>,
        f_name: &String,
        type_expr: &TypeExpr<TypeRef>,
        val: &Value,
    ) -> anyhow::Result<()> {
        match &type_expr.type_ref {
            TypeRef::ScopedName(sn) => {
                let resolver = self.ctx.resolver;
                if let Some(m_remote) = resolver.get_module(&sn.module_name) {
                    if let Some(decl) = resolver.get_decl(sn) {
                        let dvg = TsDefaultValue {
                            ctx: &ResolverModule {
                                module: m_remote,
                                resolver,
                            },
                            decl,
                            type_map: self.type_map,
                        };
                        dvg.gen_decl(t, decl, type_expr, f_name, val)?
                    } else {
                        return self.create_err_resolve_scoped_name(f_name, sn);
                    }
                } else {
                    return self.create_err_resolve_module(f_name, &sn.module_name);
                }
                // let resolver = self.ctx.resolver;
                // if let Some(remote_mod) = resolver.get_module(&sn.module_name) {
                //     if let Some(decl) = resolver.get_decl(sn) {
                //         let dvg = TsDefaultValue {
                //             ctx: &ResolverModule {
                //                 module: remote_mod,
                //                 resolver,
                //             },
                //             decl,
                //             type_map: self.type_map,
                //         };
                //         dvg.gen_decl(t, decl, type_expr, f_name, val)?
                //     } else {
                //         return self.create_err_resolve_scoped_name(f_name, sn);
                //     }
                // } else {
                //     return self.create_err_resolve_scoped_name(f_name, sn);
                // }
            }
            TypeRef::LocalName(d) => {
                // let decl = self.find_local_decl(&d);
                let sn = &ScopedName {
                    module_name: self.ctx.module.name.clone(),
                    name: d.clone(),
                };
                if let Some(decl) = self.ctx.resolver.get_decl(sn) {
                    self.gen_decl(t, decl, type_expr, f_name, val)?
                } else {
                    return self.create_err_resolve_scoped_name(f_name, sn);
                }
                // let type_params = crate::utils::ast::get_type_params(decl);

                // if type_expr.parameters.len() != type_params.len() {
                //     return self.create_err_mismatch_type_params(
                //         &decl.name,
                //         type_params.len(),
                //         type_expr.parameters.len(),
                //     );
                // }
                // let mut type_map: HashMap<String, &TypeExpr<TypeRef>> = HashMap::new();
                // for (i, tp) in type_params.iter().enumerate() {
                //     let mut te_p = type_expr.parameters.get(i).unwrap();
                //     // TODO is this enough, or does it need to be in a loop?
                //     // I don't think so, but ...
                //     // loop {
                //     if let TypeRef::TypeParam(tp0) = &te_p.type_ref {
                //         te_p = self.type_map.get(tp0).unwrap();
                //     }
                //     //     else {
                //     //         break;
                //     //     }
                //     // }
                //     type_map.insert(tp.to_string(), te_p);
                // }
                // let tsgen_te = &mut TsDefaultValue {
                //     ctx: self.ctx,
                //     type_map: &type_map,
                //     decl,
                //     // depth: Box::new(0),
                // };
                // let inner = tsgen_te.gen_type_ref(t, &f_name, val);
                // if let Err(e) = inner {
                //     return self.create_wrapped_err(f_name, e.to_string());
                // }
            }
            TypeRef::Primitive(d) => {
                self.gen_primitive(t, &f_name, d, val, &type_expr.parameters)?;
            }
            TypeRef::TypeParam(d) => {
                // if self.depth.to_be() > 30 {
                //     todo!()
                // }
                // self.depth = Box::new(self.depth.to_be()+1);
                if let Some(te) = self.type_map.get(d) {
                    self.gen_type_expr(t, f_name, te, val)?;
                } else {
                    return self.create_err_missing_type_param(&f_name, d);
                }
            }
        }
        return Ok(());
    }

    fn gen_decl(
        &self,
        t: &mut Tokens<JavaScript>,
        decl: &Decl<TypeExpr<TypeRef>>,
        type_expr: &TypeExpr<TypeRef>,
        f_name: &String,
        val: &Value,
    ) -> anyhow::Result<()> {
        let type_params = crate::utils::ast::get_type_params(decl);
        if type_expr.parameters.len() != type_params.len() {
            return self.create_err_mismatch_type_params(
                &decl.name,
                type_params.len(),
                type_expr.parameters.len(),
            );
        }
        let mut type_map: HashMap<String, &TypeExpr<TypeRef>> = HashMap::new();
        for (i, tp) in type_params.iter().enumerate() {
            let mut te_p = type_expr.parameters.get(i).unwrap();
            // TODO is this enough, or does it need to be in a loop?
            // I don't think so, but ...
            // loop {
            if let TypeRef::TypeParam(tp0) = &te_p.type_ref {
                te_p = self.type_map.get(tp0).unwrap();
            }
            //     else {
            //         break;
            //     }
            // }
            type_map.insert(tp.to_string(), te_p);
        }
        let tsgen_te = &mut TsDefaultValue {
            ctx: self.ctx,
            type_map: &type_map,
            decl,
            // depth: Box::new(0),
        };
        let inner = tsgen_te.gen_type_ref(t, &f_name, val);
        if let Err(e) = inner {
            return self.create_wrapped_err(f_name, e.to_string());
        }
        return Ok(());
    }

    fn gen_type_ref(
        &self,
        t: &mut Tokens<JavaScript>,
        f_name: &String,
        val: &Value,
    ) -> anyhow::Result<()> {
        match &self.decl.r#type {
            DeclType::Struct(ty) => {
                if let Some(obj) = val.as_object() {
                    quote_in! { *t => $OC };
                    let mut rest = false;
                    for f0 in &ty.fields {
                        let dvg = &mut TsDefaultValue {
                            ctx: self.ctx,
                            decl: self.decl,
                            type_map: &self.type_map,
                            // depth: Box::new(0),
                        };
                        if rest {
                            quote_in! { *t => ,$[' '] };
                        } else {
                            rest = true;
                        }
                        quote_in! { *t => $(&f0.name) :$[' ']}
                        dvg.gen_default_value(t, &f0, obj.get(&f0.serialized_name))?;
                        // let inner =  dvg.gen_default_value(t, &f0, obj.get(&f0.serialized_name));
                        // if let Err(e) = inner {
                        //     return self.create_wrapped_err( f_name, e.to_string());
                        // }
                    }
                    quote_in! { *t => $CC };
                } else {
                    return self.create_err("object", f_name, val);
                }
            }
            DeclType::Union(ty) => match val {
                Value::String(b_name) => {
                    if is_enum(ty) {
                        quote_in! { *t => $DQ$(b_name)$DQ };
                        return Ok(());
                    }
                    let te = ty.fields.iter().find(|f| f.serialized_name == *b_name);
                    if let Some(te0) = te {
                        match &te0.type_expr.type_ref {
                            TypeRef::Primitive(p) => match p {
                                PrimitiveType::Void => {}
                                _ => {
                                    return self.create_err_union_void_branch(b_name);
                                }
                            },
                            _ => {
                                return self.create_err_union_void_branch(b_name);
                            }
                        }
                        quote_in! { *t => $OC };
                        quote_in! { *t => kind : $DQ$(b_name)$DQ };
                        quote_in! { *t => $CC };
                    } else {
                        return self.create_err_union_te(b_name);
                    }
                }
                Value::Object(obj) => {
                    let keys: Vec<&String> = obj.keys().collect();
                    if keys.len() != 1 {
                        return self.create_err_union(f_name, val);
                    }
                    let ser_name = keys[0];
                    let b_val = obj.get(ser_name);
                    let branch = ty.fields.iter().find(|f| f.serialized_name == *ser_name);
                    if let Some(branch) = branch {
                        quote_in! { *t => $OC };
                        quote_in! { *t => kind : $DQ$(&branch.name)$DQ, value :$[' ']};
                        self.gen_default_value(t, branch, b_val)?;
                        quote_in! { *t => $CC };
                    } else {
                        return self.create_err_union_te(ser_name);
                    }
                }
                _ => return self.create_err("object or string", f_name, val),
            },
            DeclType::Type(ty) => {
                self.gen_type_expr(t, f_name, &ty.type_expr, val)?;
            }
            DeclType::Newtype(ty) => {
                // match ty.type_expr.type_ref {
                //     TypeRef::ScopedName(_) => todo!(),
                //     TypeRef::LocalName(_) => todo!(),
                //     TypeRef::Primitive(_) => todo!(),
                //     TypeRef::TypeParam(_) => todo!(),
                // }

                // self.ctx.resolver.
                // let dvg = TsDefaultValue {
                //     ctx: &ResolverModule {
                //         module: remote_mod,
                //         resolver,
                //     },
                //     decl,
                //     type_map: self.type_map,
                // };

                self.gen_type_expr(t, f_name, &ty.type_expr, val)?;
                // let tsgen_te = &mut TsDefaultValue {
                //     ctx: self.ctx,
                //     type_map: self.type_map,
                //     decl,
                //     // depth: Box::new(0),
                // };
                // let inner = tsgen_te.gen_type_ref(t, &f_name, val);
                // if let Err(e) = inner {
                //     return self.create_wrapped_err(f_name, e.to_string());
                // }
                // todo!();
            }
        };
        Ok(())
    }

    fn gen_primitive(
        &self,
        t: &mut Tokens<JavaScript>,
        f_name: &String,
        type_: &PrimitiveType,
        val: &Value,
        type_params: &Vec<TypeExpr<TypeRef>>,
    ) -> anyhow::Result<()> {
        match type_ {
            PrimitiveType::Void => {
                if let Some(_) = val.as_null() {
                    quote_in! { *t => null }
                } else {
                    return self.create_err("Void", f_name, val);
                }
            }
            PrimitiveType::Bool => {
                if let Some(v) = val.as_bool() {
                    if v {
                        quote_in! { *t => true }
                    } else {
                        quote_in! { *t => false }
                    }
                } else {
                    return self.create_err("Bool", f_name, val);
                }
            }
            PrimitiveType::Int8 => {
                if let Some(v) = val.as_i64() {
                    // TODO check bounds
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Int8", f_name, val);
                }
            }
            PrimitiveType::Int16 => {
                if let Some(v) = val.as_i64() {
                    // TODO check bounds
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Int16", f_name, val);
                }
            }
            PrimitiveType::Int32 => {
                if let Some(v) = val.as_i64() {
                    // TODO check bounds
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Int32", f_name, val);
                }
            }
            PrimitiveType::Int64 => {
                if let Some(v) = val.as_i64() {
                    // TODO check bounds
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Int64", f_name, val);
                }
            }
            PrimitiveType::Word8 => {
                if let Some(v) = val.as_u64() {
                    // TODO check bounds
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Word8", f_name, val);
                }
            }
            PrimitiveType::Word16 => {
                if let Some(v) = val.as_u64() {
                    // TODO check bounds
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Word8", f_name, val);
                }
            }
            PrimitiveType::Word32 => {
                if let Some(v) = val.as_u64() {
                    // TODO check bounds
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Word8", f_name, val);
                }
            }
            PrimitiveType::Word64 => {
                if let Some(v) = val.as_u64() {
                    // TODO check bounds
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Word8", f_name, val);
                }
            }
            PrimitiveType::Float => {
                if let Some(v) = val.as_f64() {
                    // Is there a standard JS float format?
                    let v = format!("{}", v);
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Float", f_name, val);
                }
            }
            PrimitiveType::Double => {
                if let Some(v) = val.as_f64() {
                    let v = format!("{}", v);
                    quote_in! { *t => $(v) }
                } else {
                    return self.create_err("Float", f_name, val);
                }
            }
            PrimitiveType::Json => match serde_json::to_string(val) {
                Ok(x) => {
                    quote_in! { *t => $x };
                }
                Err(e) => {
                    return self.create_err_msg("Json", f_name, val, e.to_string());
                }
            },
            PrimitiveType::ByteVector => {
                // duplicating existing adlc, but it's not quite correct.
                // needs to import b64

                // todo check valid base64 encoding
                if let Some(v) = val.as_str() {
                    quote_in! { *t => b64.toByteArray($DQ$(v)$DQ) }
                } else {
                    return self.create_err("Bytes", f_name, val);
                }
            }
            PrimitiveType::String => {
                if let Some(v) = val.as_str() {
                    quote_in! { *t => $DQ$(v)$DQ }
                } else {
                    return self.create_err("String", f_name, val);
                }
            }
            PrimitiveType::Vector => {
                if let Some(vs) = val.as_array() {
                    quote_in! { *t =>  $OSB }
                    let mut rest = false;
                    for v in vs {
                        if rest {
                            quote_in! { *t => ,$[' '] };
                        } else {
                            rest = true;
                        }
                        // we have already checked that type_params.len() == 1
                        self.gen_type_expr(t, f_name, type_params.get(0).unwrap(), v)?;
                    }
                    quote_in! { *t =>  $CSB }
                } else {
                    return self.create_err("Vector", f_name, val);
                }
            }
            PrimitiveType::StringMap => {
                if let Some(vs) = val.as_object() {
                    quote_in! { *t =>  $OC }
                    let mut rest = false;
                    // serde use a BTreeMap, same a Haskell.
                    // If `preserve_order` feature is used for serde an index map would and the order would be different.
                    for (k, v) in vs {
                        if rest {
                            quote_in! { *t => ,$[' '] };
                        } else {
                            rest = true;
                        }
                        quote_in! { *t => $DQ$(k)$DQ :$[' '] };
                        // we have already checked that type_params.len() == 1
                        self.gen_type_expr(t, f_name, type_params.get(0).unwrap(), v)?;
                    }
                    quote_in! { *t =>  $CC }
                } else {
                    return self.create_err("StringMap", f_name, val);
                }
            }
            PrimitiveType::Nullable => {
                if val.is_null() {
                    quote_in! { *t => null }
                } else {
                    self.gen_type_expr(t, f_name, type_params.get(0).unwrap(), val)?;
                }
            }
            PrimitiveType::TypeToken => {
                // TODO should this be a 'todo!()' ?

                // This is not quite correct but it is never used since 'makeXXX' are not created if there is a tokentype field (the check is transitive).
                // In the Haskell adlc the check isn't transitive.
                // The actual output should be;
                // - for primatives "ADL.texprInt64()"
                // - for localnames "texprXXX()"
                // - for scopednames ...
                if let Some(_) = val.as_null() {
                    quote_in! { *t => null }
                } else {
                    return self.create_err("TypeToken", f_name, val);
                }
            }
        }

        Ok(())
    }
}

pub fn is_enum(m: &Union<TypeExpr<TypeRef>>) -> bool {
    m.fields
        .iter()
        .find(|f| match &f.type_expr.type_ref {
            TypeRef::Primitive(p) => match p {
                PrimitiveType::Void => false,
                _ => true,
            },
            _ => true,
        })
        .is_none()
}
