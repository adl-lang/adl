use crate::adlgen::sys::adlast2 as adlast;

pub fn mk_scoped_name(mname: &str, name: &str) -> adlast::ScopedName {
    adlast::ScopedName::new(mname.to_string(), name.to_string())
}

pub fn mk_typeexpr0(type_ref: adlast::ScopedName) -> adlast::TypeExpr<adlast::ScopedName> {
    adlast::TypeExpr {
        type_ref,
        parameters: vec![],
    }
}

pub fn get_fields<T>(d: &adlast::Decl<T>) -> Option<&Vec<adlast::Field<T>>> {
    match &d.r#type {
        adlast::DeclType::Struct(s) => Some(&s.fields),
        adlast::DeclType::Union(u) => Some(&u.fields),
        adlast::DeclType::Type(_) => None,
        adlast::DeclType::Newtype(_) => None,
    }
}

pub fn get_type_params<T>(d: &adlast::Decl<T>) -> &Vec<String> {
    match &d.r#type {
        adlast::DeclType::Struct(s) => &s.type_params,
        adlast::DeclType::Union(u) => &u.type_params,
        adlast::DeclType::Type(t) => &t.type_params,
        adlast::DeclType::Newtype(n) => &n.type_params,
    }
}
