use std::{collections::HashMap, path::PathBuf, vec};

use crate::{
    adlgen::{
        sys::adlast2::{Module1, ScopedName},
    },
};


#[derive(Clone, Debug, Eq, PartialEq, PartialOrd)]
pub enum IndexEntry {
    Dir(String),
    Leaf(String),
}

impl IndexEntry {
    fn to_string(&self) -> String {
        match self {
            IndexEntry::Leaf(i) => i.to_string(),
            IndexEntry::Dir(i) => i.to_string(),
        }
    }
}

impl Ord for IndexEntry {
    fn cmp(&self, b: &Self) -> std::cmp::Ordering {
        let a = &self;

        match a {
            IndexEntry::Dir(a) => match b {
                IndexEntry::Leaf(_) => std::cmp::Ordering::Less,
                IndexEntry::Dir(b) => a.cmp(b),
            },
            IndexEntry::Leaf(a) => match b {
                IndexEntry::Leaf(b) => a.cmp(b),
                IndexEntry::Dir(_) => std::cmp::Ordering::Greater,
            },
        }
    }
}

pub fn collect_indexes(path: PathBuf, map: &mut HashMap<PathBuf, Vec<IndexEntry>>) {
    let mut it = path.ancestors().peekable();
    it.next();
    let mut name = path.file_name();
    let mut first = true;
    while let Some(a) = it.next() {
        let e = map.entry(a.to_path_buf().clone()).or_insert(vec![]);
        let n = if let Some(n) = name {
            if let Some(n) = n.to_str() {
                n.to_string()
            } else {
                "".to_string()
            }
        } else {
            "".to_string()
        };
        match e.iter().find(|x| x.to_string() == n) {
            None => {
                match first {
                    true => e.insert(0, IndexEntry::Leaf(n)),
                    false => e.insert(0, IndexEntry::Dir(n)),
                }
                first = false;
            }
            Some(_) => {}
        }
        name = a.file_name();
    }
}

pub fn get_npm_pkg(module: &Module1) -> Option<String> {
    let npm_pkg = module.annotations.0.get(&ScopedName {
        module_name: "adlc.config.typescript".to_string(),
        name: "NpmPackage".to_string(),
    });
    npm_pkg.map(|p| p.as_str().unwrap().to_string())
}

pub fn npm_pkg_import(npm_pkg2: String, module_name: String) -> String {
    let mn_parts: Vec<&str> = module_name.split(".").collect();
    let npm_parts: Vec<&str> = npm_pkg2.rsplit("/").collect();
    let mut mn = mn_parts.iter().peekable();
    let mut npm = npm_parts.iter().peekable();
    while let (Some(m), Some(n)) = (&mn.peek(), &npm.peek()) {
        if m != n {
            break;
        }
        mn.next();
        npm.next();
    }
    let mut path = npm_pkg2;
    path.push_str("/");
    while let Some(p) = mn.next() {
        path.push_str(p);
        if let Some(_) = mn.peek() {
            path.push_str("/");
        }
    }
    path
}

pub fn rel_import(strip_first:bool, same_adl_pkg: bool, src: &String, dst: &String) -> String {
    let src_v: Vec<&str> = src.split(['.']).collect();
    let src_v = &src_v[..src_v.len() - 1];
    let dst_v0: Vec<&str> = dst.split(['.']).collect();
    let last = dst_v0.last().unwrap();
    let dst_v = &dst_v0[..dst_v0.len() - 1];
    let mut src_i = src_v.iter().peekable();
    let mut dst_i = dst_v.iter().peekable();
    let mut import = String::new();
    if !same_adl_pkg {
        if src_v.len() == 0 {
            if strip_first {
                import.push_str("../");
            } else {
                import.push_str("./");
            }
        } else {
            while let Some(_) = &src_i.next() {
                import.push_str("../");
            }
        }
        while let Some(del) = &dst_i.next() {
            import.push_str(del);
            import.push_str("/");
        }
        import.push_str(last);
        return import;
    }
    if src_v.len() == 0 && dst_v.len() == 0 {
        import.push_str("./");
        import.push_str(last);
        return import;
    }
    if src_v.len() == 0 && dst_v.len() != 0 {
        if strip_first {
            import.push_str("..");
        } else {
            import.push_str(".");
        }
        dst_i.next();
        while let Some(del) = &dst_i.next() {
            import.push_str("/");
            import.push_str(del);
        }
        import.push_str("/");
        import.push_str(last);
        return import;
    }
    import.push_str(".");
    while let (Some(sel), Some(del)) = (&src_i.peek(), &dst_i.peek()) {
        if sel != del {
            break;
        }
        src_i.next();
        dst_i.next();
    }
    if dst_v.len() == 0 {
        src_i.next();
    }
    while let Some(_) = &src_i.next() {
        import.push_str("/..");
    }
    while let Some(del) = &dst_i.next() {
        import.push_str("/");
        import.push_str(del);
    }
    import.push_str("/");
    if dst_v.len() == 0 {
        import.push_str("_/");
    };
    import.push_str(last);
    import
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_collect_indexes(
        args: &[(&[&str], &[(bool, &str)])],
    ) -> HashMap<PathBuf, Vec<IndexEntry>> {
        // fn make_collect_indexes(args: Vec<(Vec<&str>, Vec<&str>)>) -> HashMap<PathBuf, Vec<String>> {
        let mut map = HashMap::new();
        for arg in args {
            let mut iter = arg.1.iter();
            let mut v = vec![];
            while let Some(a) = iter.next() {
                match a.0 {
                    false => v.push(IndexEntry::Dir(a.1.to_string())),
                    true => v.push(IndexEntry::Leaf(a.1.to_string())),
                }
            }
            let p: PathBuf = arg.0.iter().collect();
            // let v: Vec<String> = arg.1.iter().map(|s| s.to_string()).collect();
            map.insert(p, v);
        }
        map
    }

    fn make_path_bufs(args: &[&[&str]]) -> Vec<PathBuf> {
        let mut pbs = vec![];
        for arg in args {
            pbs.insert(0, arg.iter().collect());
        }
        pbs
    }

    #[test]
    fn test_collect_indexes() {
        // (name, Vec<PathBuf>, HashMap<PathBuf, Vec<String>>)
        let tests: Vec<(
            // name
            &str,
            // Vec<PathBuf>
            &[&[&str]],
            //  HashMap<PathBuf, Vec<(Leaf|Dir, String>>
            &[(&[&str], &[(bool, &str)])],
        )> = vec![
            (
                "test 00",
                &[&["common.ts"]],
                &[(&[""], &[(true, "common.ts")])],
            ),
            (
                "test 01",
                &[&["common", "db.ts"]],
                &[
                    (&[""], &[(false, "common")]),
                    (&["common"], &[(true, "db.ts")]),
                ],
            ),
            (
                "test 02",
                &[&["common.ts"], &["common", "db.ts"]],
                &[
                    (&[""], &[(true, "common.ts"), (false, "common")]),
                    (&["common"], &[(true, "db.ts")]),
                ],
            ),
            (
                "test 03",
                &[&["common.ts"], &["common", "db.ts"], &["common", "ui.ts"]],
                &[
                    (&[""], &[(true, "common.ts"), (false, "common")]),
                    (&["common"], &[(true, "db.ts"), (true, "ui.ts")]),
                ],
            ),
        ];

        for t in tests {
            let map = &mut HashMap::new();
            let exp = make_collect_indexes(t.2);
            let pbs = make_path_bufs(t.1);
            for p in pbs {
                collect_indexes(p, map);
            }
            assert_eq!(*map, exp, "{}", t.0);

            println!("{:?}", map);
        }
    }

    #[test]
    fn test_relative_import() {
        let tests: Vec<(&str, bool, &str, &str, &str)> = vec![
            ("test 00", true, "abc", "def.ghi", "../ghi"),
            (
                "test 00 - different packages",
                false,
                "abc",
                "def.ghi",
                "../def/ghi",
            ),
            ("test 00b", true, "abc", "def", "./def"),
            ("test 01", true, "scopedname.def", "scopedname.abc", "./abc"),
            (
                "test 01 - different packages",
                false,
                "scopedname.def",
                "scopedname.abc",
                "../scopedname/abc",
            ),
            (
                "test 02",
                true,
                "scopedname.def",
                "scopedname.def.abc",
                "./def/abc",
            ),
            (
                "test 02 - different packages",
                false,
                "scopedname.def",
                "scopedname.def.abc",
                "../scopedname/def/abc",
            ),
            (
                "test 03",
                true,
                "scopedname.def",
                "runtime.adl",
                "./../runtime/adl",
            ),
            (
                "test 04",
                true,
                "common.adminui.api",
                "common",
                "./../_/common",
            ),
            (
                "test 04 - different packages",
                false,
                "common.adminui.api",
                "common",
                "../../common",
            ),
            ("test 04b", true, "common", "common.strings", "../strings"),
            (
                "test 04 - different packages",
                false,
                "common",
                "common.strings",
                "../common/strings",
            ),
            (
                "test 05",
                true,
                "common.adminui.api",
                "common.db",
                "./../db",
            ),
            (
                "test 05 - different packages",
                false,
                "common.adminui.api",
                "common.db",
                "../../common/db",
            ),
            (
                "test 06",
                true,
                "common.adminui.api",
                "common.adminui",
                "./../adminui",
            ),
            (
                "test 06 - different packages",
                false,
                "common.adminui.api",
                "common.adminui",
                "../../common/adminui",
            ),
        ];

        for t in tests {
            assert_eq!(
                rel_import(true, t.1, &t.2.to_string(), &t.3.to_string()),
                t.4,
                "{}",
                t.0
            );
        }
    }
}
