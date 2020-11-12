#!/bin/bash -xe

HASKELL_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"
cd $HASKELL_DIR

ADL_STDLIB_DIR=compiler/lib/adl
CONFIG_ADL_DIR=compiler/lib/adl/adlc/config

adlcb () {
  stack exec adlc-bootstrap -- haskell --package=ADL --no-overwrite -I$ADL_STDLIB_DIR "$@"
}

# Start clean, and build the bootstrap compiler
stack clean
stack build adl-compiler-bootstrap

# Generate the haskell code for the per-language annotation types
adlcb -I $CONFIG_ADL_DIR -O compiler/src $CONFIG_ADL_DIR/haskell.adl
adlcb -I $CONFIG_ADL_DIR -O compiler/src $CONFIG_ADL_DIR/cpp.adl
adlcb -I $CONFIG_ADL_DIR -O compiler/src $CONFIG_ADL_DIR/java.adl
adlcb -I $CONFIG_ADL_DIR -O compiler/src $CONFIG_ADL_DIR/typescript.adl
adlcb -I $CONFIG_ADL_DIR -O compiler/src $CONFIG_ADL_DIR/rust.adl

# Generate ADL specified elements of the haskell runtime
adlcb -O runtime/src $ADL_STDLIB_DIR/sys/types.adl
adlcb -O runtime/src $ADL_STDLIB_DIR/sys/adlast.adl

# Build and test the compiler itself
stack build adl-compiler
stack test adl-compiler

# Generate ADL specified elements of the typescript runtime
stack exec adlc -- typescript \
 --no-overwrite \
 --exclude-ast \
 --verbose \
 -O ../typescript/runtime \
 -I $ADL_STDLIB_DIR \
 $ADL_STDLIB_DIR/sys/types.adl $ADL_STDLIB_DIR/sys/adlast.adl $ADL_STDLIB_DIR/sys/dynamic.adl

# Run some tests for each target language
stack build generated-haskell
(cd ../typescript/tests; ./run-tests.sh)
(cd ../java; gradle build; gradle test)
(cd ../rust/tests; cargo build; cargo test)
(cd ../cpp/tests; ./run-tests.sh)
