MKFILE = $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
MKDIR = $(dir $(MKFILE))

TESTOUTDIR=/tmp/adltest
MODULEPREFIX=ADL.Compiled
ADLCFLAGS=-O $(TESTOUTDIR) --moduleprefix=$(MODULEPREFIX)
ADLC=cabal-dev/bin/adlc
ADLLIBDIR=lib
GHC=GHC_PACKAGE_PATH=cabal-dev/packages-7.4.1.conf: ghc
GHCFLAGS=-i$(TESTOUTDIR)

PATH:=$(MKDIR)cabal-dev/bin:$(PATH)

EXAMPLEADLFILES=\
    examples/adl/examples/im.adl \
    examples/adl/examples/test1.adl

tests:
	$(ADLC) haskell $(ADLCFLAGS) -I lib/adl -I examples/adl $(EXAMPLEADLFILES)
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Sys/Types.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Sys/Rpc.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Examples/Im.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Examples/Test1.hs

all: utils compiler lib examples

utils:
	(cd utils && cabal-dev -s ../cabal-dev install  --force-reinstalls)

compiler:
	(cd compiler && cabal-dev -s ../cabal-dev install)

lib:
	(cd lib && cabal-dev -s ../cabal-dev install)

docs:
	(cd utils && cabal-dev -s ../cabal-dev install --force-reinstalls --enable-documentation)
	(cd lib && cabal-dev -s ../cabal-dev install --enable-documentation)

examples:
	(cd examples && cabal-dev -s ../cabal-dev install)

clean: 
	-cabal-dev ghc-pkg unregister adl-lib

	(cd compiler && cabal-dev -s ../cabal-dev clean && rm -rf dist)
	(cd utils && cabal-dev -s ../cabal-dev clean && rm -rf dist)
	(cd lib && cabal-dev -s ../cabal-dev clean && rm -rf dist)
	(cd examples && cabal-dev -s ../cabal-dev clean && rm -rf dist)

cleanext:
	-cabal-dev ghc-pkg unregister zeromq3-haskell
	(cd ext && rm -rf include lib share)

ext: zeromq3-haskell

zeromq3-haskell: ext/lib/libzmq.so.3.0.0
	 cabal-dev install zeromq3-haskell --extra-include-dirs=$(MKDIR)ext/include --extra-lib-dirs=$(MKDIR)/ext/lib --enable-documentation

ext/lib/libzmq.so.3.0.0: ext/downloads/zeromq-3.2.2.tar.gz
	(cd ext && tar -xzf downloads/zeromq-3.2.2.tar.gz)
	(cd ext/zeromq-3.2.2 && ./configure --prefix $(MKDIR)ext)
	(cd ext/zeromq-3.2.2 && make)
	(cd ext/zeromq-3.2.2 && make install)
	rm -r ext/zeromq-3.2.2

ext/downloads/zeromq-3.2.2.tar.gz:
	mkdir -p ext/downloads
	(cd ext/downloads && wget http://download.zeromq.org/zeromq-3.2.2.tar.gz)

.PHONY : examples utils compiler lib clean

