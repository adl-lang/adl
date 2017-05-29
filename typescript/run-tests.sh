#!/bin/bash

set -e

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
GENDIR=$HERE/build
HASKELLDIR=$HERE/../haskell
ADLSTDLIBDIR=$(cd $HASKELLDIR; stack exec adlc -- show --adlstdlib)

rm -rf $GENDIR
mkdir -p $GENDIR

(cd $HERE/../haskell; stack exec adlc -- typescript -O $GENDIR --include-rt --runtime-dir $GENDIR/runtime $HERE/example.adl $ADLSTDLIBDIR/sys/types.adl)
yarn
# Compile everything to check all the types, as running jest-ts doesn't actually do so :-(
./node_modules/.bin/tsc --outDir build/tsc-out *.spec.ts `find build -name '*.ts'`; rm -r build/tsc-out
yarn test
