use std::fmt;

use crate::adlgen::sys::adlast2::{self as adlast, Module0};
use crate::parser::{serializedname_scoped_name, ExplicitAnnotationRef, RawModule};

fn apply_serialized_name(m0: &mut Module0) {
    for decl in m0.decls.iter_mut() {
        // if the match variable isn't '&mut' the the branches need '(ref mut dt)'
        //eg adlast::DeclType::Struct(ref mut dt)
        match &mut decl.r#type {
            adlast::DeclType::Struct(dt) => {
                for f in dt.fields.iter_mut() {
                    if let Some(ser_value) = f.annotations.0.remove(&serializedname_scoped_name()) {
                        // currently there is a bug where a local_name SerializedName can get confussed with sys.annotation.SerializedName
                        // this is to mitigate this
                        match ser_value.as_str() {
                            Some(ser_name) => {
                                f.serialized_name = String::from(ser_name);
                            }
                            None => {
                                // TODO print warning?
                            }
                        }
                    }
                }
            }
            adlast::DeclType::Union(dt) => {
                for f in dt.fields.iter_mut() {
                    if let Some(ser_value) = f.annotations.0.remove(&serializedname_scoped_name()) {
                        // currently there is a bug where a local_name SerializedName can get confussed with sys.annotation.SerializedName
                        // this is to mitigate this
                        match ser_value.as_str() {
                            Some(ser_name) => {
                                f.serialized_name = String::from(ser_name);
                            }
                            None => {
                                // TODO print warning?
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// Attach explicit annotations to the appropriate nodes in the AST. On failure, returns
/// the nodes that could not be attached.
pub fn apply_explicit_annotations_and_serialized_name(
    raw_module: RawModule,
) -> Result<Module0, AnnotationError> {
    let (mut module0, explicit_annotations) = raw_module;
    let mut unresolved = Vec::new();

    for ea in explicit_annotations {
        let aref = find_annotations_ref(&ea.refr, &mut module0);
        match aref {
            Some(aref) => {
                if let Some(_) = aref.0.get(&ea.scoped_name) {
                    return Err(AnnotationError::Override(format!(
                        "explicit annotations can't override prefix annotation. Target '{}.{}'",
                        module0.name,
                        // ea.scoped_name.module_name,
                        ea.scoped_name.name
                    )));
                } else {
                    aref.0.insert(ea.scoped_name, ea.value);
                }
            }
            None => {
                unresolved.push(ea.refr);
            }
        }
    }

    if unresolved.is_empty() {
        apply_serialized_name(&mut module0);
        Ok(module0)
    } else {
        Err(AnnotationError::Unresolved(UnresolvedExplicitAnnotations {
            unresolved,
        }))
    }
}

/// Find the referenced annotations hashmap
fn find_annotations_ref<'a>(
    ear: &ExplicitAnnotationRef,
    module0: &'a mut Module0,
) -> Option<&'a mut adlast::Annotations> {
    match ear {
        ExplicitAnnotationRef::Module => Some(&mut module0.annotations),
        ExplicitAnnotationRef::Decl(decl_name) => {
            if let Some(decl) = module0.decls.iter_mut().find(|d| d.name == *decl_name) {
                return Some(&mut decl.annotations);
            }
            None
        }
        ExplicitAnnotationRef::Field((decl_name, field_name)) => {
            if let Some(decl) = module0.decls.iter_mut().find(|d| d.name == *decl_name) {
                let ofields = match &mut decl.r#type {
                    adlast::DeclType::Struct(s) => Some(&mut s.fields),
                    adlast::DeclType::Union(u) => Some(&mut u.fields),
                    _ => None,
                };
                if let Some(fields) = ofields {
                    let ofield = fields.iter_mut().find(|f| f.name == *field_name);
                    if let Some(field) = ofield {
                        return Some(&mut field.annotations);
                    }
                }
            }
            None
        }
    }
}

#[derive(Debug)]
pub enum AnnotationError {
    Unresolved(UnresolvedExplicitAnnotations),
    Override(String),
}

impl std::error::Error for AnnotationError {}

impl fmt::Display for AnnotationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnnotationError::Unresolved(urefs) => {
                urefs.fmt(f)?;
                // Ok(())
            }
            AnnotationError::Override(o) => {
                write!(f, "Forbidden override: {}", o)?;
                // Ok(())
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct UnresolvedExplicitAnnotations {
    pub unresolved: Vec<ExplicitAnnotationRef>,
}

impl std::error::Error for UnresolvedExplicitAnnotations {}

impl fmt::Display for UnresolvedExplicitAnnotations {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unresolved explicit annotations: ")?;
        for uref in &self.unresolved {
            match uref {
                ExplicitAnnotationRef::Module => write!(f, "<module>")?,
                ExplicitAnnotationRef::Decl(d) => write!(f, "{}", d)?,
                ExplicitAnnotationRef::Field((d, df)) => write!(f, "{}::{}", d, df)?,
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::adlgen::sys::adlast2 as adlast;
    use crate::{parser::raw_module, utils::ast::mk_scoped_name};
    use nom_locate::LocatedSpan;

    #[test]
    fn explicit_annotations_ok() {
        let rm = raw_module(LocatedSpan::new(OK_ADL)).unwrap().1;

        // We should have 3 explicit annotations to attach
        assert_eq!(rm.1.len(), 3);

        let m0 = super::apply_explicit_annotations_and_serialized_name(rm).unwrap();
        assert_eq!(
            m0.annotations.0.get(&mk_scoped_name("", "A")),
            Some(&serde_json::Value::from(1i32))
        );
        assert_eq!(
            m0.annotations.0.get(&mk_scoped_name("", "E")),
            Some(&serde_json::Value::from(6i32))
        );

        let decl = m0.decls.iter().find(|d| d.name == "Y").unwrap();
        assert_eq!(
            decl.annotations.0.get(&mk_scoped_name("", "B")),
            Some(&serde_json::Value::from(2i32))
        );
        assert_eq!(
            decl.annotations.0.get(&mk_scoped_name("", "F")),
            Some(&serde_json::Value::from(7i32))
        );

        let field = if let adlast::DeclType::Struct(s) = &decl.r#type {
            s.fields.iter().find(|f| f.name == "z")
        } else {
            None
        }
        .unwrap();
        assert_eq!(
            field.annotations.0.get(&mk_scoped_name("", "C")),
            Some(&serde_json::Value::from(3i32))
        );
        assert_eq!(
            field.annotations.0.get(&mk_scoped_name("", "G")),
            Some(&serde_json::Value::from(8i32))
        );
    }

    const OK_ADL: &str = "
@A 1
module X {
  @B 2
  struct Y {
    @C 3
    Word64 z;
  };

  annotation E 6;
  annotation Y F 7;
  annotation Y::z G 8;
}

";

    #[test]
    fn test_serialized_name() {
        let rm = raw_module(LocatedSpan::new(SER_ADL)).unwrap().1;

        assert_eq!(rm.1.len(), 1);

        let mut m1 = super::apply_explicit_annotations_and_serialized_name(rm).unwrap();

        super::apply_serialized_name(&mut m1);

        let decl = m1.decls.iter().find(|d| d.name == "Y").unwrap();

        let field_z = if let adlast::DeclType::Struct(s) = &decl.r#type {
            s.fields.iter().find(|f| f.name == "z")
        } else {
            None
        }
        .unwrap();
        assert_eq!(field_z.serialized_name, "a");

        let field_y = if let adlast::DeclType::Struct(s) = &decl.r#type {
            s.fields.iter().find(|f| f.name == "y")
        } else {
            None
        }
        .unwrap();
        assert_eq!(field_y.serialized_name, "b");
    }

    const SER_ADL: &str = "
module X {
  struct Y {
    @SerializedName \"a\"
    Word64 z;
    Int64  y;
  };

  annotation Y::y SerializedName \"b\";
}

";

    #[test]
    fn explicit_annotations_bad() {
        let rm = raw_module(LocatedSpan::new(BAD_ADL)).unwrap().1;

        // We should have 3 explicit annotations to attach
        assert_eq!(rm.1.len(), 3);

        let err = super::apply_explicit_annotations_and_serialized_name(rm).unwrap_err();

        // All of which should have failed
        match err {
            super::AnnotationError::Unresolved(err) => assert_eq!(err.unresolved.len(), 3),
            _ => assert!(false),
        }
    }

    const BAD_ADL: &str = "
module X {
  struct Y {
    Word64 z;
  };

  annotation A F 7;
  annotation A::z G 8;
  annotation Y::q G 9;
}
";
}
