
use std::fs;
use std::path::PathBuf;

use crate::utils::ast::{mk_typeexpr0, mk_scoped_name};

use super::*;
use nom::{
    error::{ErrorKind, VerboseError, VerboseErrorKind},
    Err as NomErr,
};

#[test]
fn parse_whitespace() {
  assert_parse_ws(whitespace(inp("x")), "x");
  assert_parse_ws(whitespace(inp(" x")), "x");
  assert_parse_ws(whitespace(inp("\n x")), "x");

  assert_parse_ws(whitespace(inp(" / x")),  "/ x");
  assert_parse_ws(whitespace(inp(" // x")), "");
  assert_parse_ws(whitespace(inp(" // x\ny")), "y");
  assert_parse_ws(whitespace(inp("\n// a comment\n x")), "x");
  assert_parse_ws(whitespace(inp(" /// docstring \ny")), "/// docstring \ny");
}

fn assert_parse_ws<T>(pr: Res<Input<'_>, T>, remaining: &str) 
  where T: std::fmt::Debug+PartialEq {
  if let Ok((i, _)) = pr  {
    assert_eq!(i.fragment(), &remaining);
  } else {
    panic!("Unexpected parse failure" );
  }
}


#[test]
fn parse_ident0() {
  assert_parse_eq(ident0(inp("x")), "x");
  assert_parse_eq(ident0(inp("X")), "X");
  assert_parse_eq(ident0(inp("xy_z1")), "xy_z1");
  assert_parse_eq_2(ident0(inp("xyz.")), "xyz", ".");

  assert_eq!(
    super::ident0(inp("")),
    Err(NomErr::Error(VerboseError {
      errors: vec![
          (inp(""), VerboseErrorKind::Nom(ErrorKind::Alpha)),
      ]
    }))
  );

  assert_eq!(
    super::ident0(inp("7")),
    Err(NomErr::Error(VerboseError {
      errors: vec![
          (inp("7"), VerboseErrorKind::Nom(ErrorKind::Alpha)),
      ]
    }))
  );
}

  #[test]
fn parse_module_name() {
  assert_parse_eq(module_name(inp("x")), "x".to_owned());
  assert_parse_eq_2(module_name(inp("x.y.z;")), "x.y.z".to_owned(), ";");
}

#[test]
fn parse_scoped_name() {
  assert_parse_eq(scoped_name(inp("x")), adlast::ScopedName::new("".to_string(), "x".to_string()));
  assert_parse_eq(scoped_name(inp("x.y.z")), adlast::ScopedName::new("x.y".to_string(), "z".to_string()));
}

#[test]
fn parse_import() {
  assert_parse_eq(r#import(inp("import x.y.z")), adlast::Import::ScopedName(mk_scoped_name("x.y", "z")));
  assert_parse_eq(r#import(inp("import x.y.*")), adlast::Import::ModuleName("x.y".to_owned()));
}

#[test]
fn parse_type_expr() {

  assert_parse_eq(
    type_expr(inp("a.X")), 
    mk_typeexpr0(  mk_scoped_name("a", "X"))
  );

  assert_parse_eq(
    type_expr(inp("a.X<y.z.B>")), 
    adlast::TypeExpr{
      type_ref: mk_scoped_name("a", "X"),
      parameters: vec![
        mk_typeexpr0(mk_scoped_name("y.z", "B"))
      ]
    }
  );

  assert_parse_eq(
    type_expr(inp("a.X<y.z.B,C>")), 
    adlast::TypeExpr{
      type_ref: mk_scoped_name("a", "X"),
      parameters: vec![
        mk_typeexpr0( mk_scoped_name("y.z", "B")),
        mk_typeexpr0(mk_scoped_name("", "C")),
      ]
    }
  );
}

#[test]
fn parse_decl() {

  assert_parse_eq(
    decl(inp("struct A { F f1; G f2; }")),
    adlast::Decl{
      name: "A".to_string(),
      version:  Maybe::nothing(),
      annotations:  Map::new(Vec::new()),
      r#type: adlast::DeclType::Struct(adlast::Struct{
        type_params: Vec::new(),
        fields: vec![
          adlast::Field{
            name: "f1".to_string(),
            annotations:  Map::new(Vec::new()),
            default:  Maybe::nothing(),
            serialized_name: "f1".to_string(),
            type_expr: mk_typeexpr0(mk_scoped_name("", "F")),
          },
          adlast::Field{
            name: "f2".to_string(),
            annotations:  Map::new(Vec::new()),
            default: Maybe::nothing(),
            serialized_name: "f2".to_string(),
            type_expr: mk_typeexpr0(mk_scoped_name("", "G")),
          }
        ],
      }),
    },
  )
}

#[test]
fn parse_decl_annotations() {

  assert_parse_eq(
    decl(inp("@X.Z true @Y \"xyzzy\" struct A {}")),
      adlast::Decl{ 
      name: "A".to_string(),
      version:  Maybe::nothing(),
      annotations:  Map::from_iter(vec![
        (mk_scoped_name("", "Y"), serde_json::Value::String("xyzzy".to_owned())),
        (mk_scoped_name("X", "Z"), serde_json::Value::Bool(true)),
      ]),
      r#type: adlast::DeclType::Struct(adlast::Struct{
        type_params: Vec::new(),
        fields: vec![],
      }),
    },
  )
}

#[test]
fn parse_explicit_annotations() {
  assert_parse_eq(
    explicit_annotation(inp("annotation Bool false")),
    ExplicitAnnotation{
      refr: ExplicitAnnotationRef::Module,
      scoped_name: mk_scoped_name("", "Bool"),
      value: serde_json::Value::from(false),
    },
  );

  assert_parse_eq(
    explicit_annotation(inp("annotation MyStruct Bool false")),
    ExplicitAnnotation{
      refr: ExplicitAnnotationRef::Decl("MyStruct".to_owned()),
      scoped_name: mk_scoped_name("", "Bool"),
      value: serde_json::Value::from(false),
    },
  );

  assert_parse_eq(
    explicit_annotation(inp("annotation MyStruct::f1 Bool false")),
    ExplicitAnnotation{
      refr: ExplicitAnnotationRef::Field(("MyStruct".to_owned(), "f1".to_owned())),
      scoped_name: mk_scoped_name("", "Bool"),
      value: serde_json::Value::from(false),
    },
  );
}

#[test]
fn parse_docstring() {
  assert_parse_eq(docstring(inp("  /// my doc string\n")), " my doc string");

  assert_parse_eq(
    decl(inp("/// Some doc\n struct A {}")),
    adlast::Decl{
      name: "A".to_string(),
      version: Maybe::nothing(),
      annotations:  Map::from_iter(vec![
        (docstring_scoped_name(), serde_json::Value::from(" Some doc")),
      ]),
      r#type: adlast::DeclType::Struct(adlast::Struct{
        type_params: Vec::new(),
        fields: vec![],
      }),
    },
  );

  assert_parse_eq(
    decl(inp("/// Some doc\n /// with line 2\n struct A {}")),
    adlast::Decl{
      name: "A".to_string(),
      version:  Maybe::nothing(),
      annotations:  Map::from_iter(vec![
        (mk_scoped_name("sys.annotations", "Doc"), serde_json::Value::from(" Some doc\n with line 2")),
      ]),
      r#type: adlast::DeclType::Struct(adlast::Struct{
        type_params: Vec::new(),
        fields: vec![],
      }),
    },
  );
}

#[test]
fn parse_empty_module() {
  let pm =  raw_module(inp("module x {\n}"));
  if let Ok((_i, (m, _))) = pm  {
    assert_eq!( m.name, "x".to_string());
  } else {
    panic!("Failed to parse module" );
  }
}

#[test]
fn parse_json() {
  assert_parse_eq( json(inp("null")), serde_json::Value::Null);

  assert_parse_eq( json(inp("true")), serde_json::Value::Bool(true));
  assert_parse_eq( json(inp("false")), serde_json::Value::Bool(false));
  assert_parse_eq( json(inp("true")), serde_json::Value::Bool(true));

  assert_parse_eq( json(inp("45")), serde_json::Value::from(45u32));
  assert_parse_eq( json(inp("+45")), serde_json::Value::from(45u32));
  assert_parse_eq( json(inp("-45")), serde_json::Value::from(-45i32));
  assert_parse_eq( json(inp("45.2")), serde_json::Value::from(45.2f64));
  assert_parse_eq( json(inp("+45.2")), serde_json::Value::from(45.2f64));
  assert_parse_eq( json(inp("-45.2")), serde_json::Value::from(-45.2f64));

  assert_parse_eq( json(inp("\"\"")), serde_json::Value::String("".to_string()));
  assert_parse_eq( json(inp("\"xyz\"")), serde_json::Value::String("xyz".to_string()));

  assert_parse_eq( json(inp("\"\\\"\"")), serde_json::Value::String("\"".to_string()));
  assert_parse_eq( json(inp("\"\\\\\"")), serde_json::Value::String("\\".to_string()));
  assert_parse_eq( json(inp("\"\\n\"")), serde_json::Value::String("\n".to_string()));

  assert_parse_eq( json(inp("[]")), serde_json::Value::Array(Vec::new()));
  assert_parse_eq( json(inp("[ 45 ]")), serde_json::Value::Array(vec![
    serde_json::Value::from(45u32)
  ]));

  assert_parse_eq( json(inp("{}")), serde_json::Value::Object(serde_json::Map::new()));
  assert_parse_eq( json(inp(r#" {"f1": true, "f2": null}"#)), serde_json::Value::Object(mk_json_map(vec!(
    ("f1".to_owned(), serde_json::Value::Bool(true)),
    ("f2".to_owned(), serde_json::Value::Null),
  ))));
}

  #[test]
  fn parse_test_adl_files() {
    assert_module_file_ok("../../adl/tests/test1/test1.adl");
    assert_module_file_ok("../../adl/tests/test2/test2.adl");
    assert_module_file_ok("../../adl/tests/test3/test3.adl");
    assert_module_file_ok("../../adl/tests/test4/test4.adl");
    assert_module_file_ok("../../adl/tests/test5/test5.adl");
    assert_module_file_ok("../../adl/tests/test6/test6.adl");
    assert_module_file_ok("../../adl/tests/test7/test7.adl");
    // duplicate struct name
    assert_module_file_err("../../adl/tests/test8/test8.adl");
    // versions not implemented
    assert_module_file_err("../../adl/tests/test9/test9.adl");
    assert_module_file_err("../../adl/tests/test10/test10.adl");
    assert_module_file_err("../../adl/tests/test11/test11.adl");
    assert_module_file_ok("../../adl/tests/test12/test12.adl");
    assert_module_file_ok("../../adl/tests/test13/test13.adl");
    assert_module_file_ok("../../adl/tests/test14/test14.adl");
    assert_module_file_ok("../../adl/tests/test15/test15.adl");
    assert_module_file_ok("../../adl/tests/test16/test16.adl");
    assert_module_file_ok("../../adl/tests/test16/test16.adl");
    assert_module_file_ok("../../adl/tests/test16/test2.adl");
    assert_module_file_ok("../../adl/tests/test17/test17.adl");
    assert_module_file_ok("../../adl/tests/test18/test18.adl");
    assert_module_file_ok("../../adl/tests/test19/test19.adl");
    assert_module_file_ok("../../adl/tests/test20/test20.adl");
    assert_module_file_ok("../../adl/tests/test21/test21.adl");
    assert_module_file_ok("../../adl/tests/test22/test22a.adl");
    assert_module_file_ok("../../adl/tests/test22/test22b.adl");
    assert_module_file_ok("../../adl/tests/test23/test23.adl");
    assert_module_file_ok("../../adl/tests/test24/test24.adl");
    assert_module_file_ok("../../adl/tests/test25/admin.adl");
    assert_module_file_ok("../../adl/tests/test26/test26.adl");
    assert_module_file_ok("../../adl/tests/test27/test27.adl");
    assert_module_file_ok("../../adl/tests/test27/test27a.adl");
    assert_module_file_ok("../../adl/tests/test28/test28.adl");
    assert_module_file_ok("../../adl/tests/test29/test29.adl");
  }

  fn inp (s: &str) -> Input<'_> {
    LocatedSpan::new(s)
  }
  
  fn assert_parse_eq<T>(pr: Res<Input<'_>, T>, v:T) 
    where T: std::fmt::Debug+PartialEq {
    match pr {
      Ok((i,pv)) => {
        assert_eq!(pv, v);
        assert!(i.is_empty());
      }
      Err(e) => {
        panic!("Unexpected parse failure: {}", e);

      }
    }
  }

  fn assert_parse_eq_2<T>(pr: Res<Input<'_>, T>, v:T, remaining: &str) 
    where T: std::fmt::Debug+PartialEq {
    match pr {
      Ok((i,pv)) => {
        assert_eq!(pv, v);
        assert_eq!(*i.fragment(), remaining);
      }
      Err(e) => {
        panic!("Unexpected parse failure: {}", e);

      }
    }
}

fn assert_module_file_err(path: &str) {
  let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  d.push(path);
  let content = fs::read_to_string(d).expect(&format!("Failed to read file: {}", path) );
  let content_str: &str = &content;
  let parse_result = raw_module(inp(content_str));
  match parse_result {
    Ok(_) => assert!(false, "expected a parse error"),
    Result::Err(_) => {},
  }
}

fn assert_module_file_ok(path: &str) {
  let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
  d.push(path);
  let content = fs::read_to_string(d).expect(&format!("Failed to read file: {}", path) );
  let content_str: &str = &content;
  let parse_result = raw_module(inp(content_str));
  let err =  parse_result.err().and_then(|e| {
    match e {
      Err::Error(e) => Some(e),
      Err::Failure(e) => Some(e),
      Err::Incomplete(_e) => None,
    }
  });

  assert_eq!( err, Option::None);
}

fn mk_json_map(vs: Vec<(String,serde_json::Value)>) -> serde_json::Map<String, serde_json::Value> {
  let mut map = serde_json::Map::new();
  for (k,jv) in vs {
    map.insert(k,jv);
  }
  map
}

