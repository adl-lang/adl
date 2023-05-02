use std::{borrow::Cow, path::PathBuf};

use anyhow::anyhow;

use rust_embed::{EmbeddedFile, RustEmbed};

use crate::{
    adlgen::{adlc::packaging::EmbeddedPkg, sys::adlast2::ModuleName},
    cli::StdlibOpt,
};

#[derive(RustEmbed)]
#[folder = "../../adl/stdlib/"]
struct StdlibAsset;

#[derive(RustEmbed)]
#[folder = "../../adl/adlc/"]
struct AdlcAsset;

pub fn get_file_names(em: EmbeddedPkg) -> Vec<PathBuf> {
    match em {
        EmbeddedPkg::Sys => StdlibAsset::iter().map(String::from).map(PathBuf::from).collect(),
        EmbeddedPkg::Adlc => AdlcAsset::iter().map(String::from).map(PathBuf::from).collect(),
    }
}

// fn str_from_cow(name: Cow<str>) -> PathBuf {
//     PathBuf::from(name.to_string());
//     path.to_str().unwrap().to_string()
// }

pub fn get_adl_pkg(em: EmbeddedPkg) -> Option<Cow<'static, [u8]>> {
    let d = match em {
        EmbeddedPkg::Sys => StdlibAsset::get("adl.pkg.json"),
        EmbeddedPkg::Adlc => AdlcAsset::get("adl.pkg.json"),
    };
    d.map(|d| d.data)
}

pub fn get_stdlib(em: EmbeddedPkg, mn: &ModuleName, ext: &str) -> Option<Cow<'static, [u8]>> {
    let mut fname = mn.replace(".", "/");
    fname.push_str(".adl");
    if ext != "" {
        fname.push_str("-");
        fname.push_str(ext);
    }
    let get = match em {
        EmbeddedPkg::Sys => StdlibAsset::get,
        EmbeddedPkg::Adlc => AdlcAsset::get,
    };
    if let Some(f) = get(fname.as_str()) {
        return Some(f.data);
    };
    None
}

pub(crate) fn dump_stdlib(opts: &crate::cli::DumpStdlibOpts) -> Result<(), anyhow::Error> {
    std::fs::create_dir_all(&opts.outputdir)
        .map_err(|_| anyhow!("can't create output dir '{:?}'", opts.outputdir))?;
    match opts.lib {
        StdlibOpt::Sys => {
            for name in StdlibAsset::iter() {
                fun_name(opts.outputdir.clone(), name, StdlibAsset::get)?;
            }
        }
        StdlibOpt::Adlc => {
            for name in AdlcAsset::iter() {
                fun_name(opts.outputdir.clone(), name, StdlibAsset::get)?;
            }
        }
    };
    Ok(())
}

type Getter = fn(&str) -> Option<EmbeddedFile>;

fn fun_name(mut path: PathBuf, name: Cow<'_, str>, get: Getter) -> Result<(), anyhow::Error> {
    path.push(name.as_ref());
    Ok(if let Some(data) = get(name.as_ref()) {
        std::fs::create_dir_all(path.parent().unwrap())
            .map_err(|_| anyhow!("can't create output dir for '{:?}'", &path))?;
        std::fs::write(&path, data.data.as_ref())
            .map_err(|s| anyhow!("can't write file '{:?}' error {}", &path, s))?;
    } else {
        return Err(anyhow!("could get the contents for {}", name));
    })
}
