use regex;

use std::io::Write as _;

use regex::bytes::Regex;

use std::collections::{HashMap, HashSet};
use std::fs;
use std::ops::Deref;
use std::path::{Path, PathBuf};

use anyhow::anyhow;

use genco::fmt::{self, Indentation};
use genco::prelude::*;

use crate::adlgen::adlc::packaging::{
    AdlPackageRefType, AdlWorkspace, ModuleSrc, NpmPackage, Payload1, TsRuntimeOpt,
    TsStyle, TsWriteRuntime, TypescriptGenOptions,
};
use crate::adlgen::sys::adlast2::Module1;
use crate::adlgen::sys::adlast2::{self as adlast};
use crate::adlstdlib::get_file_names;
use crate::cli::tsgen::utils::{get_npm_pkg, npm_pkg_import, IndexEntry};
use crate::processing::loader::AdlLoader;
use crate::processing::resolver::Resolver;
use crate::processing::writer::TreeWriter;

mod astgen;
mod defaultval;
mod generate;
#[cfg(test)]
mod tests;
mod utils;
use rust_embed::RustEmbed;

#[derive(RustEmbed)]
#[folder = "src/cli/tsgen/ts-runtime/"]
struct Asset;

const TSC_B64: &[u8] =
    b"import {fromByteArray as b64Encode, toByteArray as b64Decode} from 'base64-js'";
const DENO_B64: &[u8] = b"import {encode as b64Encode, decode as b64Decode} from 'https://deno.land/std@0.97.0/encoding/base64.ts'";

fn get_modules(
    opts: &TypescriptGenOptions,
    wrk_root: Option<PathBuf>,
    r#ref: AdlPackageRefType,
) -> Result<Vec<String>, anyhow::Error> {
    match r#ref {
        AdlPackageRefType::Dir(d) => match &opts.modules {
            ModuleSrc::All => {
                if wrk_root == None {
                    return Err(anyhow!("wrk_root needed when module src all specified"));
                }
                let pkg_root = wrk_root.unwrap().join(d.path.clone()).canonicalize()?;
                if let Some(pkg_root_str) = pkg_root.as_os_str().to_str() {
                    Ok(walk_and_collect_adl_modules(pkg_root_str, &pkg_root))
                } else {
                    return Err(anyhow!("Could get str from pkg_root"));
                }
            }
            ModuleSrc::Modules(ms) => Ok(ms.clone()),
        },
        AdlPackageRefType::Embedded(e) => match &opts.modules {
            ModuleSrc::Modules(ms) => Ok(ms.clone()),
            ModuleSrc::All => Ok(get_file_names(e.alias)
                .iter()
                .filter(|f| {
                    if let Some(ext) = f.extension() {
                        ext == "adl"
                    } else {
                        false
                    }
                })
                .map(|p| {
                    let mut p0 = p.clone();
                    p0.set_extension("");
                    p0.to_str().unwrap().to_string().replace("/", ".")
                })
                .collect()),
        },
    }
}

fn walk_and_collect_adl_modules(pkg_root: &str, cwd: &PathBuf) -> Vec<String> {
    let mut mods = vec![];
    if let Ok(files) = fs::read_dir(cwd) {
        for file in files {
            if let Ok(file) = file {
                let path = file.path();
                if path.is_file() {
                    if let Some(ext) = path.extension() {
                        if ext == "adl" {
                            if let Some(name) = path.to_str() {
                                let name1 = &name[(pkg_root.len() + 1)..(name.len() - 4)];
                                let name2 = name1.replace("/", ".");
                                log::info!("  adding module {}", name2);
                                mods.push(name2);
                            }
                        }
                    }
                }
                if path.is_dir() {
                    mods.append(&mut walk_and_collect_adl_modules(pkg_root, &path));
                }
            }
        }
    }
    mods
}

pub fn tsgen(
    strip_first: bool,
    packageable: bool,
    loader: Box<dyn AdlLoader>,
    opts: &TypescriptGenOptions,
    wrk_root: Option<PathBuf>,
    r#ref: AdlPackageRefType,
    dep_adl_pkgs: Vec<&Payload1>,
) -> anyhow::Result<()> {
    if opts.outputs == None {
        // not gen for this pkg
        return Ok(());
    }
    let outputs = opts.outputs.as_ref().unwrap();
    let (manifest, outputdir) = match outputs {
        crate::adlgen::adlc::packaging::OutputOpts::Gen(gen) => (
            gen.manifest.as_ref().map(|m| PathBuf::from(m)),
            PathBuf::from(gen.output_dir.clone()),
        ),
    };

    let mut resolver = Resolver::new(loader);
    let module_names = get_modules(opts, wrk_root, r#ref)?;
    for m in &module_names {
        let r = resolver.add_module(m);
        match r {
            Ok(()) => (),
            Err(e) => return Err(anyhow!("Failed to load module {}: {:?}", m, e)),
        }
    }

    let mut writer = TreeWriter::new(outputdir.clone(), manifest)?;

    let _parent = outputdir.file_name().unwrap().to_str().unwrap().to_string();

    let modules: Vec<Module1> = resolver
        .get_module_names()
        .into_iter()
        .map(|mn| resolver.get_module(&mn).unwrap())
        .collect();

    let index_map = &mut HashMap::new();

    for m in &modules {
        let does_contain = module_names.contains(&m.name);
        if opts.generate_transitive || does_contain {
            let path = path_from_module_name(strip_first, m.name.to_owned());

            utils::collect_indexes(path.clone(), index_map);

            let path = path.as_path();
            // if None == path.components().next() {
            //     return Err(anyhow!("Output module name was empty. Potential config issue 'strip_first' might need to be false. Module: '{}'. strip_first: '{}'", m.name, strip_first));
            // }
            let code = gen_ts_module(m, &resolver, opts)?;
            writer
                .write(path, code)
                .map_err(|e| anyhow!("Error write to path {:?}. Error: {}", path, e.to_string()))?;
        }
    }

    {
        let tokens = &mut js::Tokens::new();
        let dep_adl_resolvers = dep_adl_pkgs
            .iter()
            .filter_map(|d| d.p_ref.ts_opts.as_ref().map(|t| t.npm_pkg_name.clone()))
            .collect();

        if opts.include_resolver {
            gen_resolver(
                tokens,
                opts.npm_pkg_name.clone(),
                opts.generate_transitive,
                &opts.runtime_opts,
                &resolver,
                &modules,
                dep_adl_resolvers,
            )?;
            let config = js::Config::default();
            // let config = js::Config{
            //     ..Default::default()
            // };
            let mut w = fmt::IoWriter::new(Vec::<u8>::new());
            // let mut w = fmt::IoWriter::new(stdout.lock());
            let fmt = fmt::Config::from_lang::<JavaScript>();
            let fmt = fmt::Config::with_indentation(fmt, Indentation::Space(2));
            tokens.format_file(&mut w.as_formatter(&fmt), &config)?;
            let vector = w.into_inner();
            let code = std::str::from_utf8(&vector)?;
            let path = PathBuf::from("resolver.ts");
            writer.write(path.as_path(), code.to_string())?;
        }
    }

    if let TsRuntimeOpt::Generate(_) = &opts.runtime_opts {
        gen_runtime(false, false, &opts.ts_style, &mut writer)?
    }

    if packageable {
        let mut keys: Vec<&PathBuf> = index_map.keys().collect();
        keys.sort();
        for k in keys {
            let mut out = Vec::new();
            write!(&mut out, "/* @generated - key {:?} */\n", k)?;
            let vs = index_map.get(k).unwrap();
            let mut v1: Vec<IndexEntry> = vec![];
            for v in vs {
                v1.push(v.to_owned());
            }
            v1.sort();
            for v in &v1 {
                match v {
                    IndexEntry::Dir(ie) => {
                        if ie == "_" {
                            write!(&mut out, "export * from './{}/index';\n", ie)?;
                        } else {
                            write!(&mut out, "export * as {} from './{}/index';\n", ie, ie)?;
                        }
                    }
                    IndexEntry::Leaf(ie) => {
                        let iep: Vec<&str> = ie.split(".").collect();
                        let mut pat = String::from("/");
                        pat.push_str(iep[0]);
                        if k.eq(&PathBuf::from("_"))
                            && (opts.npm_pkg_name.eq(iep[0])
                                || opts.npm_pkg_name.ends_with(pat.as_str()))
                        {
                            write!(&mut out, "export * from './{}';\n", iep[0])?;
                        } else {
                            write!(&mut out, "export * as {} from './{}';\n", iep[0], iep[0])?;
                        }
                    }
                }
            }
            let path_ind = k.join("index.ts");
            let code = std::str::from_utf8(&out)?;
            writer.write(path_ind.as_path(), code.to_string())?;
        }
    }
    Ok(())
}

pub fn gen_npm_package(payload: &Payload1, wrk1: &AdlWorkspace<Payload1>) -> anyhow::Result<()> {
    let opts = payload.p_ref.ts_opts.as_ref().unwrap();

    if opts.outputs == None {
        // not gen for this pkg
        return Ok(());
    }
    let outputs = opts.outputs.as_ref().unwrap();
    let outputdir = match outputs {
        crate::adlgen::adlc::packaging::OutputOpts::Gen(gen) => {
            PathBuf::from(gen.output_dir.clone())
        }
    };
    let mut writer = TreeWriter::new(outputdir.clone(), None)?;

    let mut npm_package = NpmPackage::new(opts.npm_pkg_name.clone(), opts.npm_version.clone());
    match &opts.runtime_opts {
        TsRuntimeOpt::WorkspaceRef(rt) => {
            npm_package
                .dependencies
                .insert(rt.clone(), "workspace:*".to_string());
        }
        TsRuntimeOpt::PackageRef(rt) => {
            npm_package
                .dependencies
                .insert(rt.name.clone(), rt.version.clone());
        }
        TsRuntimeOpt::Generate(_) => {}
    };
    for (k,v) in &opts.scripts {
        npm_package.scripts.entry(k.clone()).or_insert(v.clone());
    }

    for d in &opts.extra_dependencies {
        npm_package.dependencies.insert(d.0.clone(), d.1.clone());
    }
    for d in &opts.extra_dev_dependencies {
        npm_package
            .dev_dependencies
            .insert(d.0.clone(), d.1.clone());
    }

    if !opts.generate_transitive {
        for r in payload.pkg.requires.iter() {
            match &r.r#ref {
                crate::adlgen::adlc::packaging::PkgRef::Path(p0) => {
                    match wrk1.r#use.iter().find(|p| p.pkg.path == *p0) {
                        Some(p1) => match &p1.p_ref.ts_opts {
                            Some(ts_opts) => {
                                npm_package.dependencies.insert(
                                    ts_opts.npm_pkg_name.clone(),
                                    "workspace:*".to_string(),
                                );
                            }
                            None => {
                                return Err(anyhow!(
                                "pkg_ref::path - no ts_opts in workspace file for package '{:?}'",
                                p1.p_ref
                            ))
                            }
                        },
                        None => return Err(anyhow!("no package is workspace with path '{}'", p0)),
                    }
                }
                crate::adlgen::adlc::packaging::PkgRef::Alias(a) => {
                    match wrk1
                        .r#use
                        .iter()
                        .find(|p| p.pkg.global_alias == Some(a.to_string()))
                    {
                        Some(p1) => match &p1.p_ref.ts_opts {
                            Some(ts_opts) => {
                                npm_package.dependencies.insert(
                                    ts_opts.npm_pkg_name.clone(),
                                    "workspace:*".to_string(),
                                );
                            }
                            None => {
                                return Err(anyhow!(
                                "pkg_ref::alias - no ts_opts in workspace file for package '{:?}'",
                                p1.p_ref
                            ))
                            }
                        },
                        None => {
                            if *a == "sys".to_string() {
                                npm_package
                                    .dependencies
                                    .insert("@adl-lang/sys".to_string(), "1.0.0".to_string());
                            } else {
                                return Err(anyhow!("no package is workspace with alias '{}'", a));
                            }
                        }
                    }
                }
            };
        }
    }
    let content = serde_json::to_string_pretty(&npm_package)?;
    writer.write(Path::new("package.json"), content)?;
    log::info!("generated {:?}", outputdir.clone().join("package.json"));

    if let Some(ts_config) = &opts.tsconfig {
        let content = serde_json::to_string_pretty(ts_config)?;
        writer.write(Path::new("tsconfig.json"), content)?;
        log::info!("generated {:?}", outputdir.clone().join("tsconfig.json"));    
    }

    Ok(())
}

fn gen_ts_module(
    m: &Module1,
    resolver: &Resolver,
    opts: &TypescriptGenOptions,
) -> anyhow::Result<String> {
    // TODO sys.annotations::SerializedName needs to be embedded
    let tokens = &mut js::Tokens::new();
    let adlr = match &opts.runtime_opts {
        TsRuntimeOpt::WorkspaceRef(pkg) => js::import(pkg.clone() + "/adl", "ADL").into_wildcard(),
        TsRuntimeOpt::PackageRef(pkg) => {
            js::import(pkg.name.clone() + "/adl", "ADL").into_wildcard()
        }
        TsRuntimeOpt::Generate(_gen) => {
            let src_v: Vec<&str> = m.name.split(['.']).collect();
            let src_v = &src_v[..src_v.len() - 1];
            let mut import = String::new();
            import.push_str("./");
            let mut src_i = src_v.iter().peekable();
            while src_i.next() != None {
                import.push_str("../");
            }
            import.push_str("runtime/adl");
            // TODO modify the import path with opts.runtime_dir
            js::import(import, "ADL").into_wildcard()
        }
    };
    let mut mgen = generate::TsGenVisitor {
        module: m,
        npm_pkg: &Some(opts.npm_pkg_name.clone()),
        resolver: resolver,
        adlr,
        map: &mut HashMap::new(),
        opts,
    };
    mgen.gen_module(tokens)?;
    // let stdout = std::io::stdout();
    let mut w = fmt::IoWriter::new(Vec::<u8>::new());
    // let mut w = fmt::IoWriter::new(stdout.lock());
    let fmt = fmt::Config::from_lang::<JavaScript>();
    let fmt = fmt::Config::with_indentation(fmt, Indentation::Space(2));

    let config = js::Config::default();
    // let config = js::Config{
    //     ..Default::default()
    // };
    tokens.format_file(&mut w.as_formatter(&fmt), &config)?;
    let vector = w.into_inner();
    let code = std::str::from_utf8(&vector)?;
    Ok(code.to_string())
}

fn path_from_module_name(strip_first: bool, mname: adlast::ModuleName) -> PathBuf {
    let mut path = PathBuf::new();
    let mut iter = mname.split(".").enumerate().peekable();
    while let Some((i, el)) = iter.next() {
        if strip_first && i == 0 {
            if iter.peek() == None {
                // this means the adl file is at the top level.
                path.push("_");
            } else {
                continue;
            }
        }
        path.push(el);
    }
    path.set_extension("ts");
    return path;
}

fn gen_resolver(
    t: &mut Tokens<JavaScript>,
    npm_pkg: String,
    generate_transitive: bool,
    runtime_opts: &TsRuntimeOpt,
    resolver: &Resolver,
    modules: &Vec<Module1>,
    adl_pkg_resolvers: HashSet<String>,
) -> anyhow::Result<()> {
    let mut local_keys = vec![];
    let m_imports: Vec<js::Import> = modules
        .iter()
        .map(|m| {
            let npm_pkg2 = if let Some(m2) = resolver.get_module(&m.name) {
                get_npm_pkg(&m2)
            } else {
                None
            };

            if let Some(npm_pkg2) = &npm_pkg2 {
                if adl_pkg_resolvers.contains(npm_pkg2) {
                    let alias = npm_pkg2
                        .replace("@", "")
                        .replace("-", "_")
                        .replace("/", "_");
                    return js::import(format!("{}/resolver", npm_pkg2), "ADL_local")
                        .with_alias(alias);
                }
            }
            if !generate_transitive && npm_pkg2 != None {
                let npm_pkg2 = npm_pkg2.unwrap();
                if npm_pkg2 != npm_pkg.clone() {
                    let alias = m.name.replace(".", "_");
                    local_keys.push(alias.clone());
                    return js::import(npm_pkg_import(npm_pkg2, m.name.clone()), "_AST_MAP")
                        .with_alias(alias);
                } else {
                    let name = rel_import_in_resolver(m);
                    let alias = m.name.replace(".", "_");
                    local_keys.push(alias.clone());
                    return js::import(format!("./{}", name), "_AST_MAP").with_alias(alias);
                }
            } else {
                if npm_pkg2 != Some(npm_pkg.clone()) {
                    let alias = m.name.replace(".", "_");
                    local_keys.push(alias.clone());
                    return js::import(format!("./{}", m.name.replace(".", "/")), "_AST_MAP")
                        .with_alias(alias);
                } else {
                    let name = rel_import_in_resolver(m);
                    let alias = m.name.replace(".", "_");
                    local_keys.push(alias.clone());
                    return js::import(format!("./{}", name), "_AST_MAP").with_alias(alias);
                }
            };
        })
        .collect();

    let (adlr1, adlr2) = match runtime_opts {
        TsRuntimeOpt::WorkspaceRef(pref) => (
            js::import(format!("{}/adl", pref.as_str()), "declResolver"),
            js::import(format!("{}/adl", pref.as_str()), "ScopedDecl"),
        ),
        TsRuntimeOpt::PackageRef(pref) => (
            js::import(format!("{}/adl", pref.name.as_str()), "declResolver"),
            js::import(format!("{}/adl", pref.name.as_str()), "ScopedDecl"),
        ),
        TsRuntimeOpt::Generate(_gen) => (
            js::import("./runtime/adl", "declResolver"),
            js::import("./runtime/adl", "ScopedDecl"),
        ),
    };
    let gened = "/* @generated from adl */";

    let mut dep_keys: Vec<&String> = adl_pkg_resolvers.iter().collect();
    dep_keys.sort();
    local_keys.sort();
    quote_in! { *t =>
    $gened$['\r']
    $(register (adlr2))
    $(register (adlr1))
    $(for m in m_imports => $(register (m)))


    export const ADL_local: { [key: string]: ScopedDecl } = {$['\r']
      $(for m in local_keys => ...$(m),$['\r'])
    };$['\r']

    export const ADL: { [key: string]: ScopedDecl } = {$['\r']
      ...ADL_local,
      $(for m in dep_keys => ...$(m.replace("@", "").replace("-", "_").replace("/", "_")),$['\r'])
    };$['\r']

    export const RESOLVER = declResolver(ADL);
    }

    Ok(())
}

fn rel_import_in_resolver(m: &adlast::Module<adlast::TypeExpr<adlast::TypeRef>>) -> String {
    let mut it = m.name.split(".").into_iter().peekable();
    it.next();
    let mut name = String::new();
    while let Some(n) = it.next() {
        name.push_str(n);
        if it.peek() != None {
            name.push_str("/");
        }
    }
    name
}

pub fn write_runtime(packageable: bool, rt_opts: &TsWriteRuntime) -> anyhow::Result<()> {
    let mut writer = TreeWriter::new(PathBuf::from(&rt_opts.output_dir), None)?;
    gen_runtime(true, packageable, &rt_opts.ts_style, &mut writer)?;
    Ok(())
}

fn gen_runtime(
    strip_first: bool,
    packageable: bool,
    ts_style: &TsStyle,
    writer: &mut TreeWriter,
) -> anyhow::Result<()> {
    log::info!("Writing Runtime to file system ...");
    let re = Regex::new(r"\$TSEXT").unwrap();
    let re2 = Regex::new(r"\$TSB64IMPORT").unwrap();
    for rt_name in Asset::iter() {
        // println!("  '{}'", rt_name);
        let mut file_path = PathBuf::new();
        if !strip_first {
            file_path.push("./runtime");
        }
        // file_path.push(&rt_gen_opts.runtime_dir);
        file_path.push(rt_name.as_ref());
        if !packageable {
            let is_ts = if let Some(ex) = file_path.extension() {
                ex.to_os_string().eq("ts")
            } else {
                false
            };
            if !is_ts {
                log::info!("skipping file as !packageable is set. file {:?}", file_path);
                continue;
            }
        }
        let dir_path = file_path.parent().unwrap();
        std::fs::create_dir_all(dir_path)?;

        log::info!("writing {}", file_path.display());

        let data = Asset::get(rt_name.as_ref()).unwrap();
        let content = data.data.as_ref();
        match ts_style {
            TsStyle::Tsc => {
                let content = re.replace_all(content, "".as_bytes());
                let content = re2.replace(&content, TSC_B64);
                let x = content.deref();
                let y = String::from_utf8(x.to_vec())?;
                writer.write(file_path.as_path(), y)?;
            }
            TsStyle::Deno => {
                let content = re.replace_all(content, ".ts".as_bytes());
                let content = re2.replace(&content, DENO_B64);
                let x = content.deref();
                let y = String::from_utf8(x.to_vec())?;
                writer.write(file_path.as_path(), y)?;
            }
        }
    }
    Ok(())
}
