use std::fs::{File};
use std::io::BufReader;
use std::path::{PathBuf};

use serde::Deserialize;

use crate::adlgen_dev::testing_table::{TestFilesMetaData};
use crate::cli::{AdlSearchOpts, OutputOpts};

use super::*;

#[test]
fn generate_ts_from_test_files() {
    let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    d.push("../../adl/tests/testing_table.json");

    let file = File::open(d).expect(&format!(
        "Failed to read file: {}",
        "../../adl/tests/testing_table.json"
    ));
    let reader = BufReader::new(file);

    let mut de = serde_json::Deserializer::from_reader(reader);
    match TestFilesMetaData::deserialize(&mut de) {
        Ok(tests) => {
            for t in &tests {
                if t.skip {
                    println!(
                        "Skipping {} {} - ts gen output;",
                        &t.search_path,
                        &t.title,
                    );
                    continue;
                }
                let mut sp = PathBuf::from("../../adl/tests/");
                let mut outdir = PathBuf::from("build/dev_adl");
                outdir.push(t.search_path.clone());
                let mut manifest = PathBuf::from("build/dev_adl");
                manifest.push(t.search_path.clone());
                manifest.push("manifest");

                sp.push(t.search_path.clone());
                let opts = TsOpts {
                    search: AdlSearchOpts {
                        path: vec![PathBuf::from("../../adl/stdlib"), sp],
                    },
                    output: OutputOpts {
                        outdir,
                        manifest: Some(manifest),
                    },
                    include_runtime: true,
                    modules: t.modules.clone(),
                    capitalize_branch_names_in_types: true,
                };
                // TODO consider failed.
                // t.fail
                match tsgen(&opts) {
                    Ok(_) => {
                        println!(
                            "{} {} - ts gen output;  {}",
                            &t.search_path,
                            &t.title,
                            &t.description.join("\n    "),
                        );
                        for m in &t.modules {
                            println!("  build/dev_adl/{}/{}.ts", &t.search_path, m)
                        }
                        if t.fail {
                            assert!(false, "the above test was expected to fail, but passed")
                        }
                    }
                    Err(e) => {
                        if t.fail {
                            println!(
                                "{} {} - failed as expected for src;  {}",
                                &t.search_path,
                                &t.title,
                                &t.description.join("\n\t"),
                            );
                            for m in &t.modules {
                                println!("  ../../adl/tests/{}/{}.adl", &t.search_path, m)
                            }
                        } else {
                            println!(
                                "{} {} {} - Error '{}'",
                                &t.search_path,
                                &t.title,
                                &t.description.join("\n\t"),
                                e.to_string()
                            );
                            for m in &t.modules {
                                println!("  ../../adl/tests/{}/{}.adl", &t.search_path, m)
                            }
                            assert!(false, "Error : '{:?}'\n{}", t, e.to_string());
                        }
                    }
                };
            }
        }
        Err(err) => assert!(false, "error deserializing testing_table {}", err),
    }

    // // Read the JSON contents of the file as an instance of `User`.
    // let u: Result<TestFilesMetaData, _> = serde_json::from_reader(reader);
    // match u {
    //     Ok(tests) => { dbg!(tests); },
    //     Err(err) => assert!(false, "error deserializing testing_table {}", err),
    // }
}
