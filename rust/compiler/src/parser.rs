use std::collections::HashMap;
use crate::adlgen::sys::{adlast2 as adlast};

use nom::{
  *,
  character::{
    complete::{
      multispace0,
      satisfy,
    }
  },
  multi::{
    many0,
    many0_count,
    separated_list0,
  },
  branch::alt,
  combinator::{
    opt, 
    recognize,
    map,
    value,
  },
  sequence::{
    pair,
    delimited,
    preceded,
    terminated,
  },
  error::{ 
    VerboseError,
    ParseError
  },
  character::complete::{alpha1, alphanumeric1},
  bytes::complete::tag,
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;


type TypeExpr0 = adlast::TypeExpr<adlast::ScopedName>;


/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and 
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
  where
  F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
  delimited(
    multispace0,
    inner,
    multispace0
  )
}

// Match a tag and surrounding whitespace
fn wtag<'a, E: ParseError<&'a str>>(t: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, &'a str, E>
{
  delimited(
    multispace0,
    tag(t),
    multispace0
  )
}


pub fn ident0(i: &str) -> Res<&str,&str>  {
  recognize(
    pair(
      alpha1,
      many0_count(alt((alphanumeric1, tag("_"))))
    )
  )(i)
}

pub fn module_name(i: &str) -> Res<&str,adlast::ModuleName>  {
  let (i,m) = recognize(
    pair(
      ident0,
      many0( pair( satisfy(|c| c == '.'), ident0))
    )
  )(i)?;
  Ok((i,m.to_string()))
}

pub fn scoped_name(i: &str) -> Res<&str,adlast::ScopedName>  {
  let (i,(n0,mut ns)) = 
    pair(
      ident0,
      many0( preceded( satisfy(|c| c == '.'), ident0))
  )(i)?;
  ns.insert(0, n0);
  let ns : Vec<String> = ns.iter().map(|n| n.to_string()).collect();
  let (name, module_name) = ns.split_last().unwrap();
  let scoped_name = adlast::ScopedName{
    module_name: module_name.join("."),
    name: name.clone(),
  };
  Ok((i, scoped_name))
}

pub fn module(i: &str) -> Res<&str,adlast::Module<TypeExpr0>>  {

  let (i,_) = ws(tag("module"))(i)?;
  let (i,name) = ws(module_name)(i)?;
  let (i,mut decls) = delimited(
    wtag("{"),
    many0(
      terminated(decl, wtag(";"))
    ),
    wtag("}"),
  )(i)?;
  let decls: HashMap<String,adlast::Decl<TypeExpr0>> = decls.drain(..).map( |d| (d.name.clone(),d) ).collect();

  let module = adlast::Module::new(
    name,
    Vec::new(),
    decls,
    crate::adlrt::custom::sys::types::map::Map::new(Vec::new()),
  );

  Ok( (i,module) )
}

pub fn decl(i: &str) -> Res<&str,adlast::Decl<TypeExpr0>>  {
  let (i,(name,dtype)) = decl_type(i)?;
  let decl = adlast::Decl{
    name: name.to_string(),
    r#type: dtype,
    annotations: crate::adlrt::custom::sys::types::map::Map::new(Vec::new()),
    version: crate::adlrt::custom::sys::types::maybe::Maybe::nothing()
  };
  Ok((i,decl))
}

pub fn decl_type(i: &str) -> Res<&str,(&str,adlast::DeclType<TypeExpr0>)>  {
  alt((
    map(struct_, |(name,s)| (name,adlast::DeclType::Struct(s))),
    map(union,   |(name,u)| (name,adlast::DeclType::Union(u))),
    map(typedef, |(name,t)| (name,adlast::DeclType::Type(t))),
    map(newtype, |(name,n)| (name,adlast::DeclType::Newtype(n))),
  ))(i)
}

pub fn struct_(i: &str) -> Res<&str,(&str,adlast::Struct<TypeExpr0>)>  {
  let (i,_) = wtag("struct")(i)?;
  let (i,name) = ws(ident0)(i)?;
  let (i,type_params) = type_params(i)?;
  let (i,fields) = 
    delimited(
      wtag("{"),
      many0(
        terminated(
          field,
          wtag(";"),
        )
      ),
      wtag("}"),
    )(i)?;
  let struct_ = adlast::Struct{
    fields: fields,
    type_params: type_params,
  };
  Ok((i,(name,struct_)))
}


pub fn union(i: &str) -> Res<&str,(&str,adlast::Union<TypeExpr0>)>  {
  let (i,_) = wtag("union")(i)?;
  let (i,name) = ws(ident0)(i)?;
  let (i,type_params) = type_params(i)?;
  let (i,fields) = 
    delimited(
      wtag("{"),
      many0(
        terminated(
          field,
          wtag(";"),
        )
      ),
      wtag("}"),
    )(i)?;
  let union = adlast::Union{
    fields: fields,
    type_params: type_params,
  };
  Ok((i,(name,union)))
}

pub fn typedef(i: &str) -> Res<&str,(&str,adlast::TypeDef<TypeExpr0>)>  {
  let (i,_) = wtag("type")(i)?;
  let (i,name) = ws(ident0)(i)?;
  let (i,type_params) = type_params(i)?;
  let (i,type_expr) = 
    preceded(
      wtag("="),
      type_expr,
    )(i)?;
  let typedef = adlast::TypeDef{
    type_params,
    type_expr,
  };
  Ok((i,(name,typedef)))
}

pub fn newtype(i: &str) -> Res<&str,(&str,adlast::NewType<TypeExpr0>)>  {
  let (i,_) = ws(tag("newtype"))(i)?;
  let (i,name) = ws(ident0)(i)?;
  let (i,type_params) = type_params(i)?;
  let (i,type_expr) = preceded(
    wtag("="),
    type_expr,
  )(i)?;
  let (i,default) = opt(
    preceded(
      wtag("="),
      json,
    )
  )(i)?;

  let newtype = adlast::NewType{
    type_params,
    type_expr,
    default: maybe_from_option(default),
  };
  Ok((i,(name,newtype)))
}

pub fn field(i: &str) -> Res<&str,adlast::Field<TypeExpr0>>  {
  let (i,texpr) = ws(type_expr)(i)?;
  let (i,name) = ws(ident0)(i)?;
  let (i,default) = opt( preceded( wtag("="), json))(i)?;
  let field = adlast::Field{
      name: name.to_string(),
      serialized_name: name.to_string(),
      type_expr: texpr,
      default: maybe_from_option(default),
      annotations: crate::adlrt::custom::sys::types::map::Map::new(Vec::new()),
  };
  Ok((i,field))
}

pub fn type_params(i: &str) -> Res<&str,Vec<adlast::Ident>> {
  map(
    opt(delimited(
      wtag("<"),
      separated_list0(ws(tag(",")), map( ident0, |i| i.to_string())),
      wtag(">")
    )), 
    |idents| idents.unwrap_or_else( || Vec::new() )
  )(i)
}

pub fn type_expr(i: &str) -> Res<&str,TypeExpr0> {
  map(
    pair(
      scoped_name,
      type_expr_params,
    ),
    |(tref,params)| adlast::TypeExpr::new(tref, params)
  )(i)
}

pub fn type_expr_params(i: &str) -> Res<&str,Vec<TypeExpr0>> {
  map(
    opt(delimited(
      wtag("<"),
      separated_list0(ws(tag(",")), type_expr),
      wtag(">")
    )),
    |texpr| texpr.unwrap_or_else( || Vec::new() )
  )(i)
}

pub fn json(i: &str) -> Res<&str,serde_json::Value> {
  value( serde_json::Value::Null, ws(tag("null")))(i)
}

pub fn maybe_from_option<T>(v : Option<T>) -> crate::adlrt::custom::sys::types::maybe::Maybe<T> {
  match v {
    Some(v) => crate::adlrt::custom::sys::types::maybe::Maybe::just(v),
    None => crate::adlrt::custom::sys::types::maybe::Maybe::nothing(),
  }
}



#[cfg(test)]
mod tests {
  use super::*;
  use nom::{
      error::{ErrorKind, VerboseError, VerboseErrorKind},
      Err as NomErr,
  };

  #[test]
  fn parse_ident0() {
    assert_eq!(ident0("x"), Ok(("", "x")));
    assert_eq!(ident0("X"), Ok(("", "X")));
    assert_eq!(ident0("xy_z1"), Ok(("", "xy_z1")));
    assert_eq!(ident0("xyz."), Ok((".", "xyz")));

    assert_eq!(
      super::ident0(""),
      Err(NomErr::Error(VerboseError {
        errors: vec![
            ("", VerboseErrorKind::Nom(ErrorKind::Alpha)),
        ]
      }))
    );

    assert_eq!(
      super::ident0("7"),
      Err(NomErr::Error(VerboseError {
        errors: vec![
            ("7", VerboseErrorKind::Nom(ErrorKind::Alpha)),
        ]
      }))
    );
  }

  #[test]
  fn parse_module_name() {
    assert_eq!(module_name("x"), Ok(("", "x".to_string())));
    assert_eq!(module_name("x.y.z;"), Ok((";", "x.y.z".to_string())));
  }

  #[test]
  fn parse_scoped_name() {

    assert_eq!(scoped_name("x"), Ok(("", adlast::ScopedName::new("".to_string(), "x".to_string()))));
    assert_eq!(scoped_name("x.y.z"), Ok(("", adlast::ScopedName::new("x.y".to_string(), "z".to_string()))));
  }

  #[test]
  fn parse_type_expr() {

    assert_eq!(
      type_expr("a.X"), 
      Ok(("", mk_typeexpr0(  mk_scoped_name("a", "X"))))
    );

    assert_eq!(
      type_expr("a.X<y.z.B>"), 
      Ok(("", adlast::TypeExpr{
        type_ref: mk_scoped_name("a", "X"),
        parameters: vec![
          mk_typeexpr0(mk_scoped_name("y.z", "B"))
        ]
      }))
    );

    assert_eq!(
      type_expr("a.X<y.z.B,C>"), 
      Ok(("", adlast::TypeExpr{
        type_ref: mk_scoped_name("a", "X"),
        parameters: vec![
          mk_typeexpr0( mk_scoped_name("y.z", "B")),
          mk_typeexpr0(mk_scoped_name("", "C")),
        ]
      }))
    );
  }

  #[test]
  fn parse_decl() {

    assert_eq!(
      decl("struct A { F f1; G f2; }"),
      Ok(("", adlast::Decl{
        name: "A".to_string(),
        version: mk_empty_maybe(),
        annotations: mk_empty_annotations(),
        r#type: adlast::DeclType::Struct(adlast::Struct{
          type_params: Vec::new(),
          fields: vec![
            adlast::Field{
              name: "f1".to_string(),
              annotations: mk_empty_annotations(),
              default: mk_empty_maybe(),
              serialized_name: "f1".to_string(),
              type_expr: mk_typeexpr0(mk_scoped_name("", "F")),
            },
            adlast::Field{
              name: "f2".to_string(),
              annotations: mk_empty_annotations(),
              default: mk_empty_maybe(),
              serialized_name: "f2".to_string(),
              type_expr: mk_typeexpr0(mk_scoped_name("", "G")),
            }
          ],
        }),
      })),
    )
  }

  #[test]
  fn parse_empty_module() {
    let pm =  module("module x {\n}");
    if let Ok((i, m)) = pm  {
      assert_eq!( m.name, "x".to_string());
    } else {
      panic!("Failed to parse module" );
    }
  }

  fn mk_scoped_name(mname: &str, name: &str) -> adlast::ScopedName {
    adlast::ScopedName::new(mname.to_string(), name.to_string())
  }

  fn mk_typeexpr0(type_ref: adlast::ScopedName) -> adlast::TypeExpr<adlast::ScopedName> {
    adlast::TypeExpr{type_ref, parameters: vec![]}
  }

  fn mk_empty_annotations() -> crate::adlrt::custom::sys::types::map::Map<adlast::ScopedName,serde_json::Value> {
    crate::adlrt::custom::sys::types::map::Map::new(Vec::new())
  }
  fn mk_empty_maybe<T>() -> crate::adlrt::custom::sys::types::maybe::Maybe<T> {
    crate::adlrt::custom::sys::types::maybe::Maybe::nothing()
  }
}