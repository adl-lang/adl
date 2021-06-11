#!/bin/bash -xe

HASKELL_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"
cd $HASKELL_DIR

ADL_DIR=compiler/lib/adl

adlcb () {
  stack exec adlc-bootstrap -- haskell --package=ADL --no-overwrite -I$ADL_DIR "$@"
}

# Start clean, and build the bootstrap compiler
stack clean
stack build --flag adl-compiler:bootstrap adl-compiler:adlc-bootstrap

# Generate the haskell code for the per-language annotation types
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/config/haskell.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/config/cpp.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/config/java.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/config/typescript.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/config/rust.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/codegen/types.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/codegen/java.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/codegen/batch.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/codegen/ast.adl
adlcb -O compiler/adlc-lib1 $ADL_DIR/adlc/codegen/typescript.adl

# Generate ADL specified elements of the haskell runtime
adlcb -O runtime/src $ADL_DIR/sys/types.adl
adlcb -O runtime/src $ADL_DIR/sys/adlast.adl

# Build and test the compiler itself
stack build --test adl-compiler 

# Generate ADL specified elements of the typescript runtime
# these are in the style "template" so they can be converted
# to tsc or deno style later
stack exec adlc -- typescript \
 --no-overwrite \
 --exclude-ast \
 --verbose \
 --ts-style template \
 -O ../typescript/runtime \
 -I $ADL_DIR \
 $ADL_DIR/sys/types.adl $ADL_DIR/sys/adlast.adl $ADL_DIR/sys/dynamic.adl

 # Generate a deno ADL enviroment suitable for scripting the
 # adl compiler
 stack exec adlc -- typescript \
 --ts-style deno \
 -O ../typescript/deno \
 --no-overwrite \
 --verbose \
 --include-rt \
 --include-resolver \
 --generate-transitive \
 --runtime-dir runtime \
 --manifest=../typescript/deno/.adl-manifest \
 -I $ADL_DIR \
 $ADL_DIR/adlc/codegen/batch.adl

# Generate ADL specified elements of the c++ runtime
CPP_RUNTIME_DIR=../cpp/runtime/src-generated 
stack exec adlc -- cpp \
 --no-overwrite \
 --verbose \
 --include-prefix adl \
 -O $CPP_RUNTIME_DIR \
 -I $ADL_DIR \
 $ADL_DIR/sys/types.adl $ADL_DIR/sys/adlast.adl $ADL_DIR/sys/dynamic.adl

# Run some tests for each target language
stack build generated-tests
(cd ../typescript/tests; ./run-tests.sh)
(cd ../java; gradle build; gradle test)
(cd ../rust/tests; cargo build; cargo test)
(cd ../cpp/tests; ./run-tests.sh)
