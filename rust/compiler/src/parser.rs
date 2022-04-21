// TODO: 
// parse json
// introduce cut combinator to improve error message
// annotations


use std::collections::HashMap;
use crate::adlgen::sys::{adlast2 as adlast};

use nom::{
  *,
  character::{
    complete::{
      multispace0,
      satisfy,
      none_of,
    }
  },
  multi::{
    many0,
    many0_count,
    separated_list0,
  },
  branch::alt,
  combinator::{
    cut,
    opt, 
    recognize,
    map,
    value,
  },
  number::complete::{double},
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
  bytes::complete::{ tag, is_not},
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;

type TypeExpr0 = adlast::TypeExpr<adlast::ScopedName>;


// Consumes whitespace and comments, but not docstrings
pub fn whitespace(i: &str) -> Res<&str, ()>
where
{
  let mut state = 0;
  let mut chars = i.chars();
  let mut first_slash = chars.clone();
  loop {
    let current = chars.clone();
    if let Some(c) = chars.next() {
      match state {
        0 => match c {
          '\n' => (),
          '\r' => (),
          '\t' => (),
          ' ' => (),
          '/' => {
            first_slash = current.clone();
            state = 1;
          },
          _ => return Ok((current.as_str(), ())),
        },
        1 => match c {
          '/' => {
            state = 2;
          },
          _ => return Ok((first_slash.as_str(), ())),
        }
        2 => match c {
          '/' => return Ok((first_slash.as_str(), ())),
          _ => {
            state = 3;
          }
        }
        _ => match c {
          '\n' => { state = 0 }
          '\r' => { state = 0 }
          _ => ()
        }
      }
    } else {
      return Ok( (chars.as_str(), ()));
    }
  }
}

pub fn docstring(i: &str) -> Res<&str, &str> {
  let (i,_) = preceded(whitespace, tag("///"))(i)?;
  let (i,text) = i.split_at_position_complete(|item| {
        let c = item.as_char();
        c == '\r' || c == '\n'
  })?;
  Ok((i,text))
}


/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and 
/// trailing whitespace, returning the output of `inner`.
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> Res<&'a str, O>
  where
  F: Fn(&'a str) -> Res<&'a str, O>,
{
  delimited(
    whitespace,
    inner,
    whitespace,
  )
}

// Match a tag and surrounding whitespace
fn wtag<'a>(t: &'static str) -> impl FnMut(&'a str) -> Res<&'a str, &'a str>
{
  delimited(
    whitespace,
    tag(t),
    whitespace
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
  let (i,annotations) = many0(prefix_annotation)(i)?;
  let (i,(name,dtype)) = decl_type(i)?;

  let decl = adlast::Decl{
    name: name.to_string(),
    r#type: dtype,
    annotations: merge_annotations(annotations),
    version: crate::adlrt::custom::sys::types::maybe::Maybe::nothing()
  };
  Ok((i,decl))
}

pub fn prefix_annotation(i: &str) -> Res<&str, (adlast::ScopedName, serde_json::Value)> {
  alt((
    preceded(wtag("@"), pair(scoped_name, json)),
    map(docstring, |s| (docstring_scoped_name(), serde_json::Value::from(s))),
  ))(i)
}

pub fn merge_annotations(anns: Vec<(adlast::ScopedName, serde_json::Value)>) -> adlast::Annotations {
  // Create a map out of the annotations, but join any doc strings as separate lines
  let mut hm = HashMap::new();
  let mut ds =  Vec::new();

  for (k,v) in anns {
    if k == docstring_scoped_name() {
      ds.push(v.as_str().unwrap().to_owned());
    } else {
      hm.insert(k,v);
    }
  }
  if !ds.is_empty() {
    hm.insert(docstring_scoped_name(), serde_json::Value::from(ds.join("\n")));
  }
  crate::adlrt::custom::sys::types::map::Map(hm)
}

pub fn docstring_scoped_name() -> adlast::ScopedName {
  adlast::ScopedName::new("sys.annotations".to_owned(), "Doc".to_owned())
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
  alt((
    value( serde_json::Value::Null, ws(tag("null"))),
    value( serde_json::Value::Bool(true), ws(tag("true"))),
    value( serde_json::Value::Bool(false), ws(tag("false"))),
    json_string,
    json_number,
    json_object,
    json_array,
  ))(i)
}


pub fn json_string(i: &str) -> Res<&str, serde_json::Value> {
  map( json_string0, |s: String| serde_json::Value::from(s))(i)
}

pub fn json_string0(i: &str) -> Res<&str, String> {
  let mut result = String::new();
  let mut esc = false;

  let (i,_) = wtag("\"")(i)?;

  let mut chars = i.chars();
  loop {
    if let Some(c) = chars.next() {
      if esc {
        match c {
          'b' => result.push(0x08 as char),
          'f' => result.push(0xc as char),
          'n' => result.push('\n'),
          'r' => result.push('\r'),
          't' => result.push('\t'),
          // TODO: implement \uXXXX 
          c => result.push(c),
        }
        esc = false;
      } else if c == '\\' {
        esc = true;
      } else if c == '"' {
        return Ok( (chars.as_str(), result) );
      } else {
        result.push(c);
      }
    } else {
      return Err(Err::Failure(VerboseError{errors: vec![(
        chars.as_str(),
        nom::error::VerboseErrorKind::Nom(nom::error::ErrorKind::Eof)
      )]}));
    }
  }
}

pub fn json_number(i: &str) -> Res<&str, serde_json::Value> {
  map( double, |v| {
    if v.floor() == v {
      if v >= 0.0 {
        serde_json::Value::from(v as u64)
      } else {
        serde_json::Value::from(v as i64)
      }
    } else {
     serde_json::Value::from(v)
    }
  })(i)
}

pub fn json_object(i: &str) -> Res<&str, serde_json::Value> {
  let (i,fields) = 
    preceded(
      wtag("{"), 
      cut(
        terminated(
          separated_list0(wtag(","), pair(terminated(json_string0, wtag(":")), json)),
          wtag("}")
        )
      )
    )(i)?;

  let mut map = serde_json::Map::new();
  for (k,v) in fields {
    map.insert(k,v);
  }
  Ok((i, serde_json::Value::Object(map)))
}

pub fn json_array(i: &str) -> Res<&str, serde_json::Value> {
  map(
    preceded(
      wtag("["), 
      cut(
        terminated(
          separated_list0(wtag(","), json),
          wtag("]")
        )
      )
    ),
    |jv| serde_json::Value::from(jv)
  )(i)
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
  fn parse_whitespace() {
    assert_eq!(whitespace("x"), Ok(("x", ())));
    assert_eq!(whitespace(" x"), Ok(("x", ())));
    assert_eq!(whitespace("\n x"), Ok(("x", ())));
    assert_eq!(whitespace(" / x"), Ok(("/ x", ())));
    assert_eq!(whitespace(" // x"), Ok(("", ())));
    assert_eq!(whitespace(" // x\ny"), Ok(("y", ())));
    assert_eq!(whitespace("\n// a comment\n x"), Ok(("x", ())));
    assert_eq!(whitespace(" /// docstring \ny"), Ok(("/// docstring \ny", ())));
  }



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
  fn parse_decl_annotations() {

    assert_eq!(
      decl("@X.Z true @Y \"xyzzy\" struct A {}"),
      Ok(("", adlast::Decl{
        name: "A".to_string(),
        version: mk_empty_maybe(),
        annotations:  crate::adlrt::custom::sys::types::map::Map::from_iter(vec![
          (mk_scoped_name("", "Y"), serde_json::Value::String("xyzzy".to_owned())),
          (mk_scoped_name("X", "Z"), serde_json::Value::Bool(true)),
        ]),
        r#type: adlast::DeclType::Struct(adlast::Struct{
          type_params: Vec::new(),
          fields: vec![],
        }),
      })),
    )
  }

  #[test]
  fn parse_docstring() {
    assert_eq!(docstring("  /// my doc string\n"), Ok(("\n", " my doc string")));

    assert_eq!(
      decl("/// Some doc\n struct A {}"),
      Ok(("", adlast::Decl{
        name: "A".to_string(),
        version: mk_empty_maybe(),
        annotations:  crate::adlrt::custom::sys::types::map::Map::from_iter(vec![
          (docstring_scoped_name(), serde_json::Value::from(" Some doc")),
        ]),
        r#type: adlast::DeclType::Struct(adlast::Struct{
          type_params: Vec::new(),
          fields: vec![],
        }),
      })),
    );

    assert_eq!(
      decl("/// Some doc\n /// with line 2\n struct A {}"),
      Ok(("", adlast::Decl{
        name: "A".to_string(),
        version: mk_empty_maybe(),
        annotations:  crate::adlrt::custom::sys::types::map::Map::from_iter(vec![
          (mk_scoped_name("sys.annotations", "Doc"), serde_json::Value::from(" Some doc\n with line 2")),
        ]),
        r#type: adlast::DeclType::Struct(adlast::Struct{
          type_params: Vec::new(),
          fields: vec![],
        }),
      })),
    );
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

  #[test]
  fn parse_json() {
    assert_parse_eq( json("null"), serde_json::Value::Null);

    assert_parse_eq( json("true"), serde_json::Value::Bool(true));
    assert_parse_eq( json("false"), serde_json::Value::Bool(false));

    assert_parse_eq( json("45"), serde_json::Value::from(45u32));
    assert_parse_eq( json("+45"), serde_json::Value::from(45u32));
    assert_parse_eq( json("-45"), serde_json::Value::from(-45i32));
    assert_parse_eq( json("45.2"), serde_json::Value::from(45.2f64));
    assert_parse_eq( json("+45.2"), serde_json::Value::from(45.2f64));
    assert_parse_eq( json("-45.2"), serde_json::Value::from(-45.2f64));

    assert_parse_eq( json("\"\""), serde_json::Value::String("".to_string()));
    assert_parse_eq( json("\"xyz\""), serde_json::Value::String("xyz".to_string()));
    assert_parse_eq( json("\"\\\"\""), serde_json::Value::String("\"".to_string()));
    assert_parse_eq( json("\"\\\\\""), serde_json::Value::String("\\".to_string()));
    assert_parse_eq( json("\"\\n\""), serde_json::Value::String("\n".to_string()));

    assert_parse_eq( json("[]"), serde_json::Value::Array(Vec::new()));
    assert_parse_eq( json("[ 45 ]"), serde_json::Value::Array(vec![
      serde_json::Value::from(45u32)
    ]));

    assert_parse_eq( json("{}"), serde_json::Value::Object(serde_json::Map::new()));
    assert_parse_eq( json(r#" {"f1": true, "f2": null}"#), serde_json::Value::Object(mk_json_map(vec!(
      ("f1".to_owned(), serde_json::Value::Bool(true)),
      ("f2".to_owned(), serde_json::Value::Null),
    ))));

  }

  fn assert_parse_eq<T>(  pr: Res<&str, T>, v:T) 
    where T: std::fmt::Debug+PartialEq {
    if let Ok((i, pv)) = pr  {
      assert_eq!(pv, v);
      assert!(i.is_empty());
    } else {
      panic!("Unexpected parse failure" );
    }
  }

  fn mk_json_map(vs: Vec<(String,serde_json::Value)>) -> serde_json::Map<String, serde_json::Value> {
    let mut map = serde_json::Map::new();
    for (k,jv) in vs {
      map.insert(k,jv);
    }
    map
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