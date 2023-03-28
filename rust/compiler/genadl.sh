#!/bin/bash -xe

RUST_COMPILER_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $RUST_COMPILER_DIR

ADL_STDLIB_DIR=../../haskell/compiler/lib/adl

adlc rust \
  --no-overwrite \
  --verbose \
  --generate-transitive \
  --outputdir ./src \
  --module adlgen \
  --runtime-module adlrt \
  --include-rt \
  --searchdir  $ADL_STDLIB_DIR \
  $ADL_STDLIB_DIR/sys/adlast2.adl
