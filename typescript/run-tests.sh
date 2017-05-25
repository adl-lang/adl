#!/bin/bash

HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
GENDIR=$HERE/build
HASKELLDIR=$HERE/../haskell
ADLSTDLIBDIR=$(cd $HASKELLDIR; stack exec adlc -- show --adlstdlib)

rm -rf $GENDIR
mkdir -p $GENDIR

(cd $HERE/../haskell; stack exec adlc -- typescript -O $GENDIR --include-rt --runtime-dir $GENDIR/runtime $HERE/example.adl $ADLSTDLIBDIR/sys/types.adl)
yarn; yarn test
