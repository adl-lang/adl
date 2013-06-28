MKFILE = $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
MKDIR = $(dir $(MKFILE))

TESTOUTDIR=/tmp/adltest
MODULEPREFIX=ADL.Compiled
ADLCFLAGS=-O $(TESTOUTDIR) --moduleprefix=$(MODULEPREFIX)
ADLC=cabal-dev/bin/adlc
GHC=GHC_PACKAGE_PATH=cabal-dev/packages-7.4.1.conf: ghc
GHCFLAGS=-i$(TESTOUTDIR)

PATH:=$(MKDIR)cabal-dev/bin:$(PATH)

include Makefile.srcs

all: utils compiler-bootstrap runtime compiler examples
utils : .built-utils
compiler-lib: .built-compiler-lib
compiler-bootstrap: .built-compiler-bootstrap
runtime: .built-runtime
compiler: .built-compiler
examples: .built-examples

.built-utils: $(UTILS-SRC)
	(cd utils && cabal-dev -s ../cabal-dev install  --force-reinstalls)
	touch .built-utils

.built-compiler-lib: $(COMPILER-LIB-SRC) .built-utils
	(cd compiler-lib && cabal-dev -s ../cabal-dev install  --force-reinstalls)
	touch .built-compiler-lib

.built-compiler-bootstrap: $(COMPILER-BOOTSTRAP-SRC) .built-utils .built-compiler-lib
	(cd compiler-bootstrap && cabal-dev -s ../cabal-dev install  --force-reinstalls)
	touch .built-compiler-bootstrap

.built-runtime: $(RUNTIME-SRC) .built-utils .built-compiler-bootstrap
	(cd runtime && cabal-dev -s ../cabal-dev install)
	touch .built-runtime

.built-compiler: $(COMPILER-SRC) .built-utils .built-runtime
	(cd compiler && cabal-dev -s ../cabal-dev install)
	(cd compiler/tests && ../../cabal-dev/bin/adlc-tests)
	touch .built-compiler

.built-examples: $(EXAMPLES-SRC) .built-utils .built-runtime .built-compiler
	(cd examples && cabal-dev -s ../cabal-dev install)
	touch .built-examples

boot:
	runghc genMake.hs >Makefile.srcs

docs:
	(cd utils && cabal-dev -s ../cabal-dev install --force-reinstalls --enable-documentation)
	(cd runtime && cabal-dev -s ../cabal-dev install --enable-documentation)

clean: 
	-rm -f .built-*
	-cabal-dev ghc-pkg unregister adl-compiler-lib
	-cabal-dev ghc-pkg unregister adl-runtime
	-cabal-dev ghc-pkg unregister adl-utils

	-(cd examples ; cabal-dev -s ../cabal-dev clean ; rm -rf dist)
	-(cd compiler ; cabal-dev -s ../cabal-dev clean ; rm -rf dist)
	-(cd compiler-bootstrap ; cabal-dev -s ../cabal-dev clean ; rm -rf dist)
	-(cd compiler-lib ; cabal-dev -s ../cabal-dev clean ; rm -rf dist)
	-(cd utils ; cabal-dev -s ../cabal-dev clean ; rm -rf dist)
	-(cd runtime ; cabal-dev -s ../cabal-dev clean ; rm -rf dist)

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

.PHONY : examples utils compiler-bootstrap runtime compiler clean

