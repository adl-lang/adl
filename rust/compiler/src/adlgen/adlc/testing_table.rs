// @generated from adl module adlc.testing_table

use serde::Deserialize;
use serde::Serialize;

pub type TestFilesMetaData = Vec<TestFileMetaData>;

#[derive(Clone,Debug,Deserialize,Eq,Hash,PartialEq,Serialize)]
pub struct TestFileMetaData {
  pub module_root: String,

  #[serde(default="TestFileMetaData::def_lib_paths")]
  pub lib_paths: Vec<String>,

  /**
   * If null then 'build/adlc_out/$module_root' is used
   */
  #[serde(default="TestFileMetaData::def_output_dir")]
  pub output_dir: Option<String>,

  pub modules: Vec<String>,

  #[serde(default="TestFileMetaData::def_fail")]
  pub fail: bool,

  #[serde(default="TestFileMetaData::def_skip")]
  pub skip: bool,

  #[serde(default="TestFileMetaData::def_title")]
  pub title: String,

  #[serde(default="TestFileMetaData::def_description")]
  pub description: Vec<String>,

  #[serde(default="TestFileMetaData::def_keywords")]
  pub keywords: Vec<String>,
}

impl TestFileMetaData {
  pub fn new(module_root: String, modules: Vec<String>) -> TestFileMetaData {
    TestFileMetaData {
      module_root: module_root,
      lib_paths: TestFileMetaData::def_lib_paths(),
      output_dir: TestFileMetaData::def_output_dir(),
      modules: modules,
      fail: TestFileMetaData::def_fail(),
      skip: TestFileMetaData::def_skip(),
      title: TestFileMetaData::def_title(),
      description: TestFileMetaData::def_description(),
      keywords: TestFileMetaData::def_keywords(),
    }
  }

  pub fn def_lib_paths() -> Vec<String> {
    vec![]
  }

  pub fn def_output_dir() -> Option<String> {
    None
  }

  pub fn def_fail() -> bool {
    false
  }

  pub fn def_skip() -> bool {
    false
  }

  pub fn def_title() -> String {
    "".to_string()
  }

  pub fn def_description() -> Vec<String> {
    vec![]
  }

  pub fn def_keywords() -> Vec<String> {
    vec![]
  }
}
