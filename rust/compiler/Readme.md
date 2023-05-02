# Developer notes

This document is for notes when developing this module.

## Useful commands


### Generate TS for all ADL specified in `adl/tests/testing_table.json`
```
cargo +nightly test --package compiler --lib -- cli::tsgen::tests::generate_ts_from_test_files --exact --nocapture 
```

### Generate TS for a specified ADL module

```
cargo +nightly run tsgen --searchdir ../../adl/stdlib --searchdir ../../adl/tests/test3 --outdir ts-src --manifest ts-src/manifest.json --capitalize-branch-names-in-types  test3
```

### Generate an AST using `adlc` and `cargo run`, sort the JSON and compare in VSCODE.
```
cargo +nightly run ast --searchdir ../../adl/stdlib --searchdir ../../adl/tests/test30 test30_04 | jq -S . > build/ast/test30_04.rust.sorted.ast.json
adlc ast --searchdir ../../adl/stdlib  --searchdir ../../adl/tests/test4 --combined-output=build/ast/test30_04.adlc.ast.json ../../adl/tests/test30/test30_04.adl
cd build/ast
jq -S . test30_04.adlc.ast.json > test30_04.adlc.sorted.ast.json
code -d test30_04.adlc.sorted.ast.json test30_04.rust.sorted.ast.json
```

### Testing Workspace Gen
```
cargo +nightly run gen ../../adl/tests/test31/proj

cargo +nightly run gen -f adl.work1.json  ../../adl/tests/test31/proj

cargo +nightly run gen -f adl.work2.json  ../../adl/tests/test31/proj

cargo +nightly run gen -f adl.work3.json  ../../adl/tests/test31/proj
```

### Cross compile for linux from mac

```
rustup target add x86_64-unknown-linux-musl
RUSTFLAGS="-Clink-self-contained=yes -Clinker=rust-lld" cargo build --release --target x86_64-unknown-linux-musl
```