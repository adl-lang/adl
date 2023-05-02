use anyhow::anyhow;
use nom::Finish;
use nom_locate::LocatedSpan;
use std::collections::HashMap;
use std::fs;
use std::io::ErrorKind;
use std::path::PathBuf;

use crate::adlgen::adlc::packaging::{
    EmbeddedPkg, InjectAnnotation, InjectAnnotations, LoaderRefType, LoaderWorkspace, AdlPackage,
};
use crate::adlgen::sys::adlast2::{self as adlast, Module0};
use crate::parser::{convert_error, raw_module};
use crate::processing::annotations::apply_explicit_annotations_and_serialized_name;

pub fn loader_from_workspace(root: PathBuf, workspace: LoaderWorkspace) -> Box<dyn AdlLoader> {
    Box::new(WorkspaceLoader {
        root,
        workspace,
        loaders: HashMap::new(),
    })
}

pub fn loader_from_search_paths(paths: &Vec<PathBuf>) -> Box<dyn AdlLoader> {
    let loaders = paths.iter().map(loader_from_dir_tree).collect();
    Box::new(MultiLoader::new(loaders))
}

pub fn loader_from_dir_tree(path: &PathBuf) -> Box<dyn AdlLoader> {
    Box::new(DirTreeLoader::new(path.clone()))
}

pub trait AdlLoader {
    /// Find and load the specified ADL module
    fn load(
        &mut self,
        module_name: &adlast::ModuleName,
    ) -> Result<Option<(Module0,Option<AdlPackage>, Option<InjectAnnotations>)>, anyhow::Error>;
    fn debug(&self);
}

pub struct WorkspaceLoader {
    root: PathBuf,
    workspace: LoaderWorkspace,
    loaders: HashMap<String, Box<dyn AdlLoader>>,
}

impl AdlLoader for WorkspaceLoader {
    fn debug(&self) {
        println!("!!!! WorkspaceLoader {:?} {:?}", self.root, self.workspace);
        let keys: &Vec<&String> =  &self.loaders.keys().collect();
        // let &mut keys: &mut Vec<&String> = &self.loaders.keys().collect();
        // keys.sort();
        for k in keys.iter() {
            self.loaders.get(k.clone()).unwrap().debug();
        }
    }

    fn load(
        &mut self,
        module_name: &adlast::ModuleName,
    ) -> Result<Option<(Module0,Option<AdlPackage>, Option<InjectAnnotations>)>, anyhow::Error> {
        // if let Some(embedded_sys_loader_ref) = &self.workspace.embedded_sys_loader.0 {
        //     if let Some(mut module) = self.embedded.load(module_name)? {
        //         if module_name != "sys.annotations" {
        //             for an in &embedded_sys_loader_ref.loader_inject_annotate {
        //                 match an {
        //                     InjectAnnotation::Module(man) => {
        //                         module
        //                             .0
        //                             .annotations
        //                             .0
        //                             .insert(man.0 .0.clone(), man.0 .1.clone());
        //                     }
        //                 }
        //             }
        //         }
        //         module.1 = Some(embedded_sys_loader_ref.resolver_inject_annotate.clone());
        //         return Ok(Some(module));
        //     }
        // }
        for pkg in &self.workspace.r#use {
            let loader = match pkg.r#ref.clone() {
                LoaderRefType::Dir(d) => {
                    let pkg_path = d.path.as_str();
                    // println!("  looking for {} in {} or {:?}", module_name, pkg_path, pkg.0.1.global_alias.clone());
                    let pkg_name = if module_name.starts_with(pkg_path) {
                        Some(d.path.clone())
                    } else if let Some(alias) = d.global_alias.clone() {
                        if module_name.starts_with(alias.as_str()) {
                            Some(alias)
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    if pkg_name == None {
                        continue;
                    }
                    let name = pkg_name.unwrap();
                    let loader = self
                        .loaders
                        .entry(name.clone())
                        .or_insert(Box::new(DirTreeLoader::new(self.root.join(&d.path))));
                    loader
                },
                LoaderRefType::Embedded(e) => {
                    self
                    .loaders
                    .entry(format!("{:?}", e.alias))
                    .or_insert(Box::new(EmbeddedStdlibLoader { pkg: e.alias }))
                },
            };
            let module0 = loader.load(module_name);
            match module0 {
                Ok(module2) => {
                    if let Some((mut module1, _, _)) = module2.clone() {
                        for an in &pkg.loader_inject_annotate {
                            match an {
                                InjectAnnotation::Module(man) => {
                                    if *module_name != "sys.annotations".to_string() {
                                        if man.0 .0.module_name == *module_name {
                                            module1.annotations.0.insert(
                                                adlast::ScopedName {
                                                    module_name: "".to_string(),
                                                    name: man.0 .0.name.clone(),
                                                },
                                                man.0 .1.clone(),
                                            );
                                        } else {
                                            module1
                                                .annotations
                                                .0
                                                .insert(man.0 .0.clone(), man.0 .1.clone());
                                        }
                                    }
                                }
                            }
                        }
                        return Ok(Some((module1, Some(pkg.pkg.clone()), Some(pkg.resolver_inject_annotate.clone()))));
                    } else {
                        // return Ok(None);
                        // return Err(anyhow!("Module not found '{}'", module_name));
                    }
                }
                Err(_) => return module0,
            }
        }
        Ok(None)
    }
}

/// Combines a bunch of loaders
pub struct MultiLoader {
    loaders: Vec<Box<dyn AdlLoader>>,
}

impl MultiLoader {
    pub fn new(loaders: Vec<Box<dyn AdlLoader>>) -> Self {
        MultiLoader { loaders }
    }
}

impl AdlLoader for MultiLoader {
    fn debug(&self) {
        println!("!!!! MultiLoader");
        for l in &self.loaders {
            l.debug();
        }
    }

    fn load(
        &mut self,
        module_name: &adlast::ModuleName,
    ) -> Result<Option<(Module0,Option<AdlPackage>, Option<InjectAnnotations>)>, anyhow::Error> {
        for loader in &mut self.loaders {
            if let Some(module) = loader.load(module_name)? {
                return Ok(Some(module));
            }
        }
        Ok(None)
    }
}

pub struct EmbeddedStdlibLoader {
    pkg: EmbeddedPkg,
}

impl AdlLoader for EmbeddedStdlibLoader {
    fn debug(&self) {
        println!("!!! EmbeddedStdlibLoader {:?}", self.pkg);
    }

    fn load(
        &mut self,
        module_name: &adlast::ModuleName,
    ) -> Result<Option<(Module0,Option<AdlPackage>, Option<InjectAnnotations>)>, anyhow::Error> {
        if let Some((pkg, data)) = crate::adlstdlib::get_stdlib(&self.pkg, module_name, "") {
            match std::str::from_utf8(data.as_ref()) {
                Ok(content) => return parse(&content).map(|m| Some((m, Some(pkg), None))),
                Err(err) => return Err(anyhow::Error::from(err)),
            }
        } else {
            Ok(None)
        }
    }
}

pub struct DirTreeLoader {
    root: PathBuf,
    // pkg:Option<AdlPackage>,
}

impl DirTreeLoader {
    pub fn new(
        root: PathBuf, 
        // pkg:Option<AdlPackage>,
    ) -> Self {
        DirTreeLoader { 
            root,
            // pkg,
        }
    }
}

impl AdlLoader for DirTreeLoader {
    fn debug(&self) {
        println!("!!!! DirTreeLoader {:?}", self.root);
    }

    fn load(
        &mut self,
        module_name: &adlast::ModuleName,
    ) -> Result<Option<(Module0,Option<AdlPackage>, Option<InjectAnnotations>)>, anyhow::Error> {
        let mut path = self.root.clone();
        for mp in module_name.split(".") {
            path.push(mp);
        }
        path.set_extension("adl");
        let econtent = fs::read_to_string(path.clone());
        let content = match econtent {
            Err(err) => match err.kind() {
                ErrorKind::NotFound => return Ok(None),
                _ => return Err(anyhow::Error::from(err)),
            },
            Ok(content) => content,
        };
        log::info!("loaded {} from {}", module_name, path.display());
        parse(&content).map(|m| Some((m, None, None)))
    }
}

fn parse(content: &str) -> Result<Module0, anyhow::Error> {
    let inp = LocatedSpan::new(content);
    let (_, raw_module) = raw_module(inp)
        .finish()
        .map_err(|e| anyhow!(convert_error(inp, e)))?;
    match apply_explicit_annotations_and_serialized_name(raw_module) {
        Ok(module0) => Ok(module0),
        Err(err) => Err(anyhow::Error::from(err)),
    }
}
