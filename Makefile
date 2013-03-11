MKFILE = $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))

TESTOUTDIR=/tmp/adltest
MODULEPREFIX=ADL.Compiled
ADLCFLAGS=-O $(TESTOUTDIR) --moduleprefix=$(MODULEPREFIX)
ADLC=cabal-dev/bin/adlc
ADLLIBDIR=lib
GHC=GHC_PACKAGE_PATH=cabal-dev/packages-7.4.1.conf: ghc
GHCFLAGS=-i$(TESTOUTDIR)

PATH:=$(dir $(MKFILE))cabal-dev/bin:$(PATH)

EXAMPLEADLFILES=\
    examples/adl/examples/im.adl \
    examples/adl/examples/test1.adl

tests:
	$(ADLC) haskell $(ADLCFLAGS) -I lib/adl -I examples/adl $(EXAMPLEADLFILES)
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Sys/Types.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Sys/Rpc.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Examples/Im.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Examples/Test1.hs

all: compiler lib examples

compiler:
	(cd compiler && cabal-dev -s ../cabal-dev install)

lib:
	(cd lib && cabal-dev -s ../cabal-dev install)

examples:
	(cd examples && cabal-dev -s ../cabal-dev install)

clean: 
	(cd compiler && cabal-dev -s ../cabal-dev clean)

	(cd lib && cabal-dev -s ../cabal-dev clean)
	cabal-dev ghc-pkg unregister adl-lib

	(cd examples && cabal-dev -s ../cabal-dev clean)

.PHONY : examples compiler lib clean
