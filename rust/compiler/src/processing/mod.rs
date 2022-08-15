use crate::adlgen::sys::{adlast2 as adlast};
use crate::parser::{RawModule, ExplicitAnnotationRef};

type TypeExpr0 = adlast::TypeExpr<adlast::ScopedName>;
pub type Module0 = adlast::Module<TypeExpr0>;

pub trait AdlLoader {
  /// Find and load the specified ADL module
  fn load(module_name: adlast::ModuleName) -> Result<Option<RawModule>, anyhow::Error>;
}

/// Attach explicit annotations to the appropriate nodes in the AST. On failure, returns
/// the nodes that could not be attached.
pub fn apply_explicit_annotations(raw_module: RawModule) -> Result<Module0, Vec<UnResolvedExplicitAnnotation>> {
  let (mut module0, explicit_annotations) = raw_module;
  let mut unresolved = Vec::new();

  for ea in explicit_annotations {
    let aref = find_annotations_ref(&ea.refr, &mut module0);
    match aref {
      Some(aref) => { aref.0.insert(ea.scoped_name,ea.value); }
      None => { unresolved.push(ea.refr); }
    }
  }

  if unresolved.is_empty() {
    Ok(module0)
  } else {
    Err(unresolved)
  }
}

/// Find the referenced annotations hashmap
fn find_annotations_ref<'a>(ear: &ExplicitAnnotationRef, module0: &'a mut Module0) -> Option<&'a mut adlast::Annotations> {
  match ear {
    ExplicitAnnotationRef::Module => {
      Some(&mut module0.annotations)
    }
    ExplicitAnnotationRef::Decl(decl_name) => {
      if let Some(decl) = module0.decls.get_mut(decl_name) {
        return Some(&mut decl.annotations);
      }
      None
    }
    ExplicitAnnotationRef::Field((decl_name, field_name)) => {
      if let Some(decl) = module0.decls.get_mut(decl_name) {
        let ofields = match &mut decl.r#type {
          adlast::DeclType::Struct(s) => Some(&mut s.fields),
          adlast::DeclType::Union(u) => Some(&mut u.fields),
          _ => None,
        };
        if let Some(fields) = ofields {
          let ofield = fields.iter_mut().find(|f| f.name.value == *field_name);
          if let Some(field) = ofield {
           return  Some(&mut field.annotations);
          }
        }
      }
      None
    }
  }
}

type UnResolvedExplicitAnnotation = ExplicitAnnotationRef;
