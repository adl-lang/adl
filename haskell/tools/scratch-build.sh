#!/bin/bash -xe

HASKELL_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." >/dev/null 2>&1 && pwd )"
cd $HASKELL_DIR

ADL_STDLIB_DIR=$HASKELL_DIR/../adl/stdlib

adlcb () {
  stack exec adlc-bootstrap -- haskell --package=ADL --no-overwrite -I$ADL_STDLIB_DIR "$@"
}

# Start clean, and build the bootstrap compiler
stack clean
stack build adl-lib0:adlc-bootstrap

# Generate the haskell code for the per-language annotation types
adlcb -I $ADL_STDLIB_DIR -O compiler/src adlc.config.haskell
adlcb -I $ADL_STDLIB_DIR -O compiler/src adlc.config.cpp
adlcb -I $ADL_STDLIB_DIR -O compiler/src adlc.config.java
adlcb -I $ADL_STDLIB_DIR -O compiler/src adlc.config.typescript
adlcb -I $ADL_STDLIB_DIR -O compiler/src adlc.config.rust
adlcb -I $ADL_STDLIB_DIR -O compiler/src adlc.package

# Generate ADL specified elements of the haskell runtime
adlcb -O runtime/src -I $ADL_STDLIB_DIR sys.types
adlcb -O runtime/src -I $ADL_STDLIB_DIR sys.adlast

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
 -O ../typescript/runtime/embedded \
 -I $ADL_STDLIB_DIR \
 sys.types sys.adlast sys.dynamic
stack exec adlc -- typescript \
 --no-overwrite \
 --exclude-ast \
 --verbose \
 --ts-style deno \
 -O ../typescript/runtime/published/src \
 -I $ADL_STDLIB_DIR \
 sys.types sys.adlast sys.dynamic

# Generate ADL specified elements of the c++ runtime
CPP_RUNTIME_DIR=../cpp/runtime/src-generated 
stack exec adlc -- cpp \
 --no-overwrite \
 --verbose \
 --include-prefix adl \
 -O $CPP_RUNTIME_DIR \
 -I $ADL_STDLIB_DIR \
 sys.types sys.adlast sys.dynamic

# Run some tests for each target language
stack build generated-tests
(cd ../typescript/tests; ./run-tests.sh)
(cd ../java; gradle build; gradle test)
(cd ../rust/tests; cargo build; cargo test)
(cd ../cpp/tests; ./run-tests.sh)
