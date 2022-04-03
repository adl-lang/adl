use std::collections::HashMap;
use crate::adlgen::sys::adlast;

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
  },
  sequence::{
    pair,
    delimited,
    preceded,
  },
  error::{ 
    VerboseError,
    ParseError
  },
  character::complete::{alpha1, alphanumeric1},
  bytes::complete::tag,
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;



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
}

pub fn module(i: &str) -> Res<&str,adlast::Module>  {

  let (i,_) = ws(tag("module"))(i)?;
  let (i,name) = ws(module_name)(i)?;
  let (i,_) = ws(tag("{"))(i)?;
  let (i,_) = ws(tag("}"))(i)?;

  let module = adlast::Module::new(
    name,
    Vec::new(),
    HashMap::new(),
    crate::adlrt::custom::sys::types::map::Map::new(Vec::new()),
  );

  Ok( (i,module) )
}

pub fn struct_(i: &str) -> Res<&str,adlast::Struct>  {
  let (i,_) = ws(tag("struct"))(i)?;
  let (i,name) = ws(ident0)(i)?;
  let (i,type_params) = type_params(i)?;
  let (i,fields) = 
    delimited(
      ws(tag("{")),
      many0(field),
      ws(tag("}")),
    )(i)?;
  let struct_ = adlast::Struct{
    fields: fields,
    type_params: type_params,
  };
  Ok((i,struct_))
}

pub fn field(i: &str) -> Res<&str,adlast::Field>  {
  let (i,texpr) = ws(type_expr)(i)?;
  let (i,name) = ws(ident0)(i)?;
  let (i,default) = opt( preceded( ws(tag("=")), json))(i)?;
  let (i,_) = ws(tag(";"))(i)?;
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
      ws(tag("<")),
      separated_list0(ws(tag(",")), map( ident0, |i| i.to_string())),
      ws(tag(">"))
    )), 
    |idents| idents.unwrap_or_else( || Vec::new() )
  )(i)
}

pub fn type_expr(i: &str) -> Res<&str,adlast::TypeExpr> {
  map(
    pair(
      type_ref,
      type_expr_params,
    ),
    |(tref,params)| adlast::TypeExpr::new(tref, params)
  )(i)
}

pub fn type_ref(i: &str) -> Res<&str,adlast::TypeRef> {
}

pub fn type_expr_params(i: &str) -> Res<&str,Vec<adlast::TypeExpr>> {
  map(
    opt(delimited(
      ws(tag("<")),
      separated_list0(ws(tag(",")), type_expr),
      ws(tag(">"))
    )),
    |texpr| texpr.unwrap_or_else( || Vec::new() )
  )(i)
}

pub fn json(i: &str) -> Res<&str,serde_json::Value> {
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
  fn parse_module() {
    let pm =  module("module x {\n}");
    if let Ok((i, m)) = pm  {
      assert_eq!( m.name, "x".to_string());
    } else {
      panic!("Failed to parse module" );
    }
  }
}