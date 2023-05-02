// @generated from adl module adlc.packaging

use crate::adlgen::sys::adlast2::Module1;
use crate::adlgen::sys::adlast2::ScopedName;
use crate::adlrt::custom::sys::types::pair::Pair;
use serde::Deserialize;
use serde::Serialize;

pub type AdlWorkspace0 = AdlWorkspace<Payload0>;

pub type AdlWorkspace1 = AdlWorkspace<Payload1>;

pub type AdlWorkspace2 = AdlWorkspace<Payload2>;

pub type LoaderWorkspace = AdlWorkspace<LoaderRef>;

pub type Payload0 = AdlPackageRef;

#[derive(Clone,Debug,Deserialize,Eq,PartialEq,Serialize)]
pub struct Payload1 {
  pub p_ref: AdlPackageRef,

  pub pkg: AdlPackage,
}

impl Payload1 {
  pub fn new(p_ref: AdlPackageRef, pkg: AdlPackage) -> Payload1 {
    Payload1 {
      p_ref: p_ref,
      pkg: pkg,
    }
  }
}

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct Payload2 {
  pub p_ref: AdlPackageRef,

  pub pkg: AdlPackage,

  pub modules: Vec<Module1>,
}

impl Payload2 {
  pub fn new(p_ref: AdlPackageRef, pkg: AdlPackage, modules: Vec<Module1>) -> Payload2 {
    Payload2 {
      p_ref: p_ref,
      pkg: pkg,
      modules: modules,
    }
  }
}

/**
 * Expected to live in a file named `adl.work.json`
 */
#[derive(Clone,Debug,Deserialize,Eq,PartialEq,Serialize)]
pub struct AdlWorkspace<T> {
  pub adlc: String,

  #[serde(rename="use")]
  pub r#use: Vec<T>,

  #[serde(default="AdlWorkspace::<T>::def_named_options")]
  pub named_options: std::collections::HashMap<String,NamedOpts>,

  #[serde(default="AdlWorkspace::<T>::def_runtimes")]
  pub runtimes: Vec<RuntimeOpts>,
}

impl<T> AdlWorkspace<T> {
  pub fn new(adlc: String, r#use: Vec<T>) -> AdlWorkspace<T> {
    AdlWorkspace {
      adlc: adlc,
      r#use: r#use,
      named_options: AdlWorkspace::<T>::def_named_options(),
      runtimes: AdlWorkspace::<T>::def_runtimes(),
    }
  }

  pub fn def_named_options() -> std::collections::HashMap<String,NamedOpts> {
    [].iter().cloned().collect()
  }

  pub fn def_runtimes() -> Vec<RuntimeOpts> {
    vec![]
  }
}

#[derive(Clone,Debug,Deserialize,Eq,PartialEq,Serialize)]
pub struct NamedOpts {
  #[serde(default="NamedOpts::def_ts_opts")]
  pub ts_opts: TsNamedOpts,
}

impl NamedOpts {
  pub fn new() -> NamedOpts {
    NamedOpts {
      ts_opts: NamedOpts::def_ts_opts(),
    }
  }

  pub fn def_ts_opts() -> TsNamedOpts {
    TsNamedOpts{runtime_opts : None, scripts : None, dependencies : None, dev_dependencies : None, tsconfig : None}
  }
}

#[derive(Clone,Debug,Deserialize,Eq,PartialEq,Serialize)]
pub struct TsNamedOpts {
  #[serde(default="TsNamedOpts::def_runtime_opts")]
  pub runtime_opts: Option<TsRuntimeOpt>,

  #[serde(default="TsNamedOpts::def_scripts")]
  pub scripts: Option<std::collections::HashMap<String,String>>,

  #[serde(default="TsNamedOpts::def_dependencies")]
  pub dependencies: Option<std::collections::HashMap<String,String>>,

  #[serde(default="TsNamedOpts::def_dev_dependencies")]
  pub dev_dependencies: Option<std::collections::HashMap<String,String>>,

  #[serde(default="TsNamedOpts::def_tsconfig")]
  pub tsconfig: Option<TsConfig>,
}

impl TsNamedOpts {
  pub fn new() -> TsNamedOpts {
    TsNamedOpts {
      runtime_opts: TsNamedOpts::def_runtime_opts(),
      scripts: TsNamedOpts::def_scripts(),
      dependencies: TsNamedOpts::def_dependencies(),
      dev_dependencies: TsNamedOpts::def_dev_dependencies(),
      tsconfig: TsNamedOpts::def_tsconfig(),
    }
  }

  pub fn def_runtime_opts() -> Option<TsRuntimeOpt> {
    None
  }

  pub fn def_scripts() -> Option<std::collections::HashMap<String,String>> {
    None
  }

  pub fn def_dependencies() -> Option<std::collections::HashMap<String,String>> {
    None
  }

  pub fn def_dev_dependencies() -> Option<std::collections::HashMap<String,String>> {
    None
  }

  pub fn def_tsconfig() -> Option<TsConfig> {
    None
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum RuntimeOpts {
  #[serde(rename="ts_runtime")]
  TsRuntime(TsWriteRuntime),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct TsWriteRuntime {
  pub output_dir: String,

  #[serde(default="TsWriteRuntime::def_referenceable")]
  pub referenceable: ReferenceableScopeOption,

  #[serde(default="TsWriteRuntime::def_npm_pkg_name")]
  pub npm_pkg_name: String,

  #[serde(default="TsWriteRuntime::def_ts_style")]
  pub ts_style: TsStyle,
}

impl TsWriteRuntime {
  pub fn new(output_dir: String) -> TsWriteRuntime {
    TsWriteRuntime {
      output_dir: output_dir,
      referenceable: TsWriteRuntime::def_referenceable(),
      npm_pkg_name: TsWriteRuntime::def_npm_pkg_name(),
      ts_style: TsWriteRuntime::def_ts_style(),
    }
  }

  pub fn def_referenceable() -> ReferenceableScopeOption {
    ReferenceableScopeOption::Local
  }

  pub fn def_npm_pkg_name() -> String {
    "@adl-lang/runtime".to_string()
  }

  pub fn def_ts_style() -> TsStyle {
    TsStyle::Tsc
  }
}

/**
 * The struct in AdlWorkspace::use required by the WorkspaceLoader
 */
#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub struct LoaderRef {
  #[serde(rename="ref")]
  pub r#ref: LoaderRefType,

  #[serde(default="LoaderRef::def_loader_inject_annotate")]
  pub loader_inject_annotate: InjectAnnotations,

  #[serde(default="LoaderRef::def_resolver_inject_annotate")]
  pub resolver_inject_annotate: InjectAnnotations,
}

impl LoaderRef {
  pub fn new(r#ref: LoaderRefType) -> LoaderRef {
    LoaderRef {
      r#ref: r#ref,
      loader_inject_annotate: LoaderRef::def_loader_inject_annotate(),
      resolver_inject_annotate: LoaderRef::def_resolver_inject_annotate(),
    }
  }

  pub fn def_loader_inject_annotate() -> InjectAnnotations {
    vec![]
  }

  pub fn def_resolver_inject_annotate() -> InjectAnnotations {
    vec![]
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum LoaderRefType {
  #[serde(rename="dir")]
  Dir(DirLoaderRef),

  #[serde(rename="embedded")]
  Embedded(EmbeddedLoaderRef),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct DirLoaderRef {
  pub path: String,

  #[serde(default="DirLoaderRef::def_global_alias")]
  pub global_alias: Option<String>,
}

impl DirLoaderRef {
  pub fn new(path: String) -> DirLoaderRef {
    DirLoaderRef {
      path: path,
      global_alias: DirLoaderRef::def_global_alias(),
    }
  }

  pub fn def_global_alias() -> Option<String> {
    None
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct EmbeddedLoaderRef {
  pub alias: EmbeddedPkg,
}

impl EmbeddedLoaderRef {
  pub fn new(alias: EmbeddedPkg) -> EmbeddedLoaderRef {
    EmbeddedLoaderRef {
      alias: alias,
    }
  }
}

pub type InjectAnnotations = Vec<InjectAnnotation>;

#[derive(Clone,Debug,Deserialize,PartialEq,Serialize)]
pub enum InjectAnnotation {
  #[serde(rename="module_")]
  Module(Pair<ScopedName, serde_json::Value>),
}

#[derive(Clone,Debug,Deserialize,Eq,PartialEq,Serialize)]
pub struct AdlPackageRef {
  #[serde(rename="ref")]
  pub r#ref: AdlPackageRefType,

  #[serde(default="AdlPackageRef::def_named_opts")]
  pub named_opts: Option<String>,

  #[serde(default="AdlPackageRef::def_ts_opts")]
  pub ts_opts: Option<TypescriptGenOptions>,
}

impl AdlPackageRef {
  pub fn new(r#ref: AdlPackageRefType) -> AdlPackageRef {
    AdlPackageRef {
      r#ref: r#ref,
      named_opts: AdlPackageRef::def_named_opts(),
      ts_opts: AdlPackageRef::def_ts_opts(),
    }
  }

  pub fn def_named_opts() -> Option<String> {
    None
  }

  pub fn def_ts_opts() -> Option<TypescriptGenOptions> {
    None
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum AdlPackageRefType {
  #[serde(rename="dir")]
  Dir(DirectoryRef),

  /**
   * An ADL module embed in the ADL compiler
   */
  #[serde(rename="embedded")]
  Embedded(EmbeddedRef),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct EmbeddedRef {
  pub alias: EmbeddedPkg,
}

impl EmbeddedRef {
  pub fn new(alias: EmbeddedPkg) -> EmbeddedRef {
    EmbeddedRef {
      alias: alias,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum EmbeddedPkg {
  #[serde(rename="sys")]
  Sys,

  #[serde(rename="adlc")]
  Adlc,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct DirectoryRef {
  pub path: String,
}

impl DirectoryRef {
  pub fn new(path: String) -> DirectoryRef {
    DirectoryRef {
      path: path,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,PartialEq,Serialize)]
pub struct TypescriptGenOptions {
  pub npm_pkg_name: String,

  #[serde(default="TypescriptGenOptions::def_npm_version")]
  pub npm_version: String,

  #[serde(default="TypescriptGenOptions::def_extra_dependencies")]
  pub extra_dependencies: std::collections::HashMap<String,VersionSpec>,

  #[serde(default="TypescriptGenOptions::def_extra_dev_dependencies")]
  pub extra_dev_dependencies: std::collections::HashMap<String,VersionSpec>,

  #[serde(default="TypescriptGenOptions::def_outputs")]
  pub outputs: Option<OutputOpts>,

  #[serde(default="TypescriptGenOptions::def_tsconfig")]
  pub tsconfig: Option<TsConfig>,

  #[serde(default="TypescriptGenOptions::def_scripts")]
  pub scripts: std::collections::HashMap<String,String>,

  #[serde(default="TypescriptGenOptions::def_runtime_opts")]
  pub runtime_opts: TsRuntimeOpt,

  #[serde(default="TypescriptGenOptions::def_generate_transitive")]
  pub generate_transitive: bool,

  #[serde(default="TypescriptGenOptions::def_include_resolver")]
  pub include_resolver: bool,

  #[serde(default="TypescriptGenOptions::def_ts_style")]
  pub ts_style: TsStyle,

  #[serde(default="TypescriptGenOptions::def_modules")]
  pub modules: ModuleSrc,

  #[serde(default="TypescriptGenOptions::def_capitalize_branch_names_in_types")]
  pub capitalize_branch_names_in_types: bool,

  #[serde(default="TypescriptGenOptions::def_capitalize_type_names")]
  pub capitalize_type_names: bool,
}

impl TypescriptGenOptions {
  pub fn new(npm_pkg_name: String) -> TypescriptGenOptions {
    TypescriptGenOptions {
      npm_pkg_name: npm_pkg_name,
      npm_version: TypescriptGenOptions::def_npm_version(),
      extra_dependencies: TypescriptGenOptions::def_extra_dependencies(),
      extra_dev_dependencies: TypescriptGenOptions::def_extra_dev_dependencies(),
      outputs: TypescriptGenOptions::def_outputs(),
      tsconfig: TypescriptGenOptions::def_tsconfig(),
      scripts: TypescriptGenOptions::def_scripts(),
      runtime_opts: TypescriptGenOptions::def_runtime_opts(),
      generate_transitive: TypescriptGenOptions::def_generate_transitive(),
      include_resolver: TypescriptGenOptions::def_include_resolver(),
      ts_style: TypescriptGenOptions::def_ts_style(),
      modules: TypescriptGenOptions::def_modules(),
      capitalize_branch_names_in_types: TypescriptGenOptions::def_capitalize_branch_names_in_types(),
      capitalize_type_names: TypescriptGenOptions::def_capitalize_type_names(),
    }
  }

  pub fn def_npm_version() -> String {
    "1.0.0".to_string()
  }

  pub fn def_extra_dependencies() -> std::collections::HashMap<String,VersionSpec> {
    [].iter().cloned().collect()
  }

  pub fn def_extra_dev_dependencies() -> std::collections::HashMap<String,VersionSpec> {
    [].iter().cloned().collect()
  }

  pub fn def_outputs() -> Option<OutputOpts> {
    None
  }

  pub fn def_tsconfig() -> Option<TsConfig> {
    None
  }

  pub fn def_scripts() -> std::collections::HashMap<String,String> {
    [].iter().cloned().collect()
  }

  pub fn def_runtime_opts() -> TsRuntimeOpt {
    TsRuntimeOpt::PackageRef(NpmPackageRef{name : "@adl-lang/runtime".to_string(), version : "^1.0.0".to_string()})
  }

  pub fn def_generate_transitive() -> bool {
    false
  }

  pub fn def_include_resolver() -> bool {
    false
  }

  pub fn def_ts_style() -> TsStyle {
    TsStyle::Tsc
  }

  pub fn def_modules() -> ModuleSrc {
    ModuleSrc::All
  }

  pub fn def_capitalize_branch_names_in_types() -> bool {
    true
  }

  pub fn def_capitalize_type_names() -> bool {
    true
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum TsRuntimeOpt {
  #[serde(rename="workspace_ref")]
  WorkspaceRef(String),

  #[serde(rename="package_ref")]
  PackageRef(NpmPackageRef),

  #[serde(rename="generate")]
  Generate(TsGenRuntime),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct TsGenRuntime {
}

impl TsGenRuntime {
  pub fn new() -> TsGenRuntime {
    TsGenRuntime {
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum OutputOpts {
  #[serde(rename="gen")]
  Gen(GenOutput),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct GenOutput {
  #[serde(default="GenOutput::def_referenceable")]
  pub referenceable: ReferenceableScopeOption,

  pub output_dir: String,

  #[serde(default="GenOutput::def_manifest")]
  pub manifest: Option<String>,
}

impl GenOutput {
  pub fn new(output_dir: String) -> GenOutput {
    GenOutput {
      referenceable: GenOutput::def_referenceable(),
      output_dir: output_dir,
      manifest: GenOutput::def_manifest(),
    }
  }

  pub fn def_referenceable() -> ReferenceableScopeOption {
    ReferenceableScopeOption::Local
  }

  pub fn def_manifest() -> Option<String> {
    None
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum TsStyle {
  #[serde(rename="tsc")]
  Tsc,

  #[serde(rename="deno")]
  Deno,
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum ModuleSrc {
  #[serde(rename="all")]
  All,

  #[serde(rename="modules")]
  Modules(Vec<String>),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum ReferenceableScopeOption {
  /**
   * Generated code will only be referred internal to the repo
   */
  #[serde(rename="local")]
  Local,

  /**
   * Generated code can be published via a package manager (e.g. npm)
   */
  #[serde(rename="remote")]
  Remote,
}

/**
 * Expected to live in a file named `adl.pkg.json`
 */
#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct AdlPackage {
  pub path: String,

  #[serde(default="AdlPackage::def_global_alias")]
  pub global_alias: Option<String>,

  /**
   * Version
   */
  pub adlc: String,

  #[serde(default="AdlPackage::def_requires")]
  pub requires: Vec<Require>,

  #[serde(default="AdlPackage::def_excludes")]
  pub excludes: Vec<Exclude>,

  #[serde(default="AdlPackage::def_replaces")]
  pub replaces: Vec<Replace>,

  #[serde(default="AdlPackage::def_retracts")]
  pub retracts: Vec<Retract>,
}

impl AdlPackage {
  pub fn new(path: String, adlc: String) -> AdlPackage {
    AdlPackage {
      path: path,
      global_alias: AdlPackage::def_global_alias(),
      adlc: adlc,
      requires: AdlPackage::def_requires(),
      excludes: AdlPackage::def_excludes(),
      replaces: AdlPackage::def_replaces(),
      retracts: AdlPackage::def_retracts(),
    }
  }

  pub fn def_global_alias() -> Option<String> {
    None
  }

  pub fn def_requires() -> Vec<Require> {
    vec![]
  }

  pub fn def_excludes() -> Vec<Exclude> {
    vec![]
  }

  pub fn def_replaces() -> Vec<Replace> {
    vec![]
  }

  pub fn def_retracts() -> Vec<Retract> {
    vec![]
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct PackageDirective {
  pub path: String,

  #[serde(default="PackageDirective::def_repo")]
  pub repo: Option<String>,
}

impl PackageDirective {
  pub fn new(path: String) -> PackageDirective {
    PackageDirective {
      path: path,
      repo: PackageDirective::def_repo(),
    }
  }

  pub fn def_repo() -> Option<String> {
    None
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Require {
  #[serde(rename="ref")]
  pub r#ref: PkgRef,

  #[serde(default="Require::def_version")]
  pub version: Option<String>,

  #[serde(default="Require::def_indirect")]
  pub indirect: bool,
}

impl Require {
  pub fn new(r#ref: PkgRef) -> Require {
    Require {
      r#ref: r#ref,
      version: Require::def_version(),
      indirect: Require::def_indirect(),
    }
  }

  pub fn def_version() -> Option<String> {
    None
  }

  pub fn def_indirect() -> bool {
    false
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub enum PkgRef {
  #[serde(rename="path")]
  Path(String),

  #[serde(rename="alias")]
  Alias(String),
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Exclude {
  pub path: String,

  pub version: String,
}

impl Exclude {
  pub fn new(path: String, version: String) -> Exclude {
    Exclude {
      path: path,
      version: version,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Replace {
  pub path: String,

  pub version: Option<String>,
}

impl Replace {
  pub fn new(path: String, version: Option<String>) -> Replace {
    Replace {
      path: path,
      version: version,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct Retract {
  pub version: String,

  #[serde(default="Retract::def_comment")]
  pub comment: Option<String>,
}

impl Retract {
  pub fn new(version: String) -> Retract {
    Retract {
      version: version,
      comment: Retract::def_comment(),
    }
  }

  pub fn def_comment() -> Option<String> {
    None
  }
}

#[derive(Clone,Debug,Deserialize,Eq,PartialEq,Serialize)]
pub struct NpmPackage {
  pub name: String,

  pub version: String,

  #[serde(default="NpmPackage::def_scripts")]
  pub scripts: std::collections::HashMap<String,String>,

  #[serde(default="NpmPackage::def_dependencies")]
  pub dependencies: std::collections::HashMap<String,String>,

  #[serde(default="NpmPackage::def_dev_dependencies")]
  #[serde(rename="devDependencies")]
  pub dev_dependencies: std::collections::HashMap<String,String>,
}

impl NpmPackage {
  pub fn new(name: String, version: String) -> NpmPackage {
    NpmPackage {
      name: name,
      version: version,
      scripts: NpmPackage::def_scripts(),
      dependencies: NpmPackage::def_dependencies(),
      dev_dependencies: NpmPackage::def_dev_dependencies(),
    }
  }

  pub fn def_scripts() -> std::collections::HashMap<String,String> {
    [].iter().cloned().collect()
  }

  pub fn def_dependencies() -> std::collections::HashMap<String,String> {
    [].iter().cloned().collect()
  }

  pub fn def_dev_dependencies() -> std::collections::HashMap<String,String> {
    [].iter().cloned().collect()
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct TsConfig {
  pub extends: Option<String>,

  pub include: Vec<String>,

  pub exclude: Vec<String>,

  #[serde(rename="compilerOptions")]
  pub compiler_options: TsCompilerOptions,
}

impl TsConfig {
  pub fn new(extends: Option<String>, include: Vec<String>, exclude: Vec<String>, compiler_options: TsCompilerOptions) -> TsConfig {
    TsConfig {
      extends: extends,
      include: include,
      exclude: exclude,
      compiler_options: compiler_options,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct TsCompilerOptions {
  #[serde(rename="outDir")]
  pub out_dir: String,

  pub lib: Vec<String>,
}

impl TsCompilerOptions {
  pub fn new(out_dir: String, lib: Vec<String>) -> TsCompilerOptions {
    TsCompilerOptions {
      out_dir: out_dir,
      lib: lib,
    }
  }
}

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct NpmPackageRef {
  pub name: String,

  pub version: VersionSpec,
}

impl NpmPackageRef {
  pub fn new(name: String, version: VersionSpec) -> NpmPackageRef {
    NpmPackageRef {
      name: name,
      version: version,
    }
  }
}

pub type VersionSpec = String;
