use std::collections::HashMap;

use crate::adlgen::sys::adlast2 as adlast;
use crate::utils::ast::{get_fields, get_type_params};

use super::ErrorConsumer;

pub mod check_duplicates {
    use super::*;

    pub fn module<T>(m: &adlast::Module<adlast::TypeExpr<T>>, ec: &mut dyn ErrorConsumer) {
        for d in &m.decls {
            decl(&m.name, d, ec);
        }
    }

    pub fn decl<T>(mname: &str, d: &adlast::Decl<adlast::TypeExpr<T>>, ec: &mut dyn ErrorConsumer) {
        if let Some(fs) = get_fields(d) {
            let dnames = find_duplicates(fs.iter().map(|f| f.name.clone()).collect());
            for dname in dnames {
                ec.consume_error(format!(
                    "decl {}.{} has duplicated field {}",
                    mname, d.name, dname
                ));
            }
        }
        let dnames = find_duplicates(get_type_params(d).clone());
        for dname in dnames {
            ec.consume_error(format!(
                "decl {}.{} has duplicated type parameter {}",
                mname, d.name, dname
            ));
        }
    }

    fn find_duplicates(strings: Vec<String>) -> Vec<String> {
        let mut hm = HashMap::new();
        for s in strings {
            *hm.entry(s).or_insert(0) += 1;
        }
        let duplicates = hm
            .into_iter()
            .filter(|(_, n)| n > &1)
            .map(|(s, _)| s)
            .collect();
        duplicates
    }
}
