
use std::fs;
  use std::path::PathBuf;

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
  fn parse_import() {
    assert_eq!(r#import("import x.y.z"), Ok(("",adlast::Import::ScopedName(mk_scoped_name("x.y", "z")))));
    assert_eq!(r#import("import x.y.*"), Ok(("",adlast::Import::ModuleName("x.y".to_owned()))));
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
      })),
    )
  }

  #[test]
  fn parse_decl_annotations() {

    assert_eq!(
      decl("@X.Z true @Y \"xyzzy\" struct A {}"),
      Ok(("", adlast::Decl{
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
      })),
    )
  }

  #[test]
  fn parse_explicit_annotations() {
    assert_eq!(
      explicit_annotation("annotation Bool false"),
      Ok(("", ExplicitAnnotation{
        refr: ExplicitAnnotationRef::Module,
        scoped_name: mk_scoped_name("", "Bool"),
        value: serde_json::Value::from(false),
      })),
    );

    assert_eq!(
      explicit_annotation("annotation MyStruct Bool false"),
      Ok(("", ExplicitAnnotation{
        refr: ExplicitAnnotationRef::Decl("MyStruct".to_owned()),
        scoped_name: mk_scoped_name("", "Bool"),
        value: serde_json::Value::from(false),
      })),
    );

    assert_eq!(
      explicit_annotation("annotation MyStruct.f1 Bool false"),
      Ok(("", ExplicitAnnotation{
        refr: ExplicitAnnotationRef::Field(("MyStruct".to_owned(), "f1".to_owned())),
        scoped_name: mk_scoped_name("", "Bool"),
        value: serde_json::Value::from(false),
      })),
    );
  }

  #[test]
  fn parse_docstring() {
    assert_eq!(docstring("  /// my doc string\n"), Ok(("\n", " my doc string")));

    assert_eq!(
      decl("/// Some doc\n struct A {}"),
      Ok(("", adlast::Decl{
        name: "A".to_string(),
        version: Maybe::nothing(),
        annotations:  Map::from_iter(vec![
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
        version:  Maybe::nothing(),
        annotations:  Map::from_iter(vec![
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
    if let Ok((i, (m, _))) = pm  {
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


  #[test]
  fn parse_test_adl_files() {
    assert_module_file_ok("../../haskell/compiler/tests/test1/input/test.adl");
    assert_module_file_ok("../../haskell/compiler/tests/test2/input/test.adl");
    assert_module_file_ok("../../haskell/compiler/tests/test3/input/test.adl");
    assert_module_file_ok("../../haskell/compiler/tests/test4/input/test.adl");
    assert_module_file_ok("../../haskell/compiler/tests/test5/input/test.adl");
    assert_module_file_ok("../../haskell/compiler/tests/test6/input/test.adl");
    assert_module_file_ok("../../haskell/compiler/tests/test7/input/test.adl");
    assert_module_file_ok("../../haskell/compiler/tests/test8/input/test.adl");
    // assert_module_file_ok("../../haskell/compiler/tests/test9/input/test.adl");
    // assert_module_file_ok("../../haskell/compiler/tests/test10/input/test.adl");
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

  fn assert_module_file_ok(path: &str) {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push(path);
    let content = fs::read_to_string(d).expect(&format!("Failed to read file: {}", path) );
    let content_str: &str = &content;
    let parse_result = module(content_str);
    let err =  parse_result.err().and_then(|e| {
      match e {
        Err::Error(e) => Some(e),
        Err::Failure(e) => Some(e),
        Err::Incomplete(e) => None,
      }
    });
    assert_eq!( err.map(|e| convert_error(content_str, e)), Option::None);
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
