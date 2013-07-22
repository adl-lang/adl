MKFILE = $(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
MKDIR = $(dir $(MKFILE))

TESTOUTDIR=/tmp/adltest
MODULEPREFIX=ADL.Compiled
ADLCFLAGS=-O $(TESTOUTDIR) --moduleprefix=$(MODULEPREFIX)
ADLC=cabal-dev/bin/adlc
GHC=GHC_PACKAGE_PATH=cabal-dev/packages-7.4.1.conf: ghc
GHCFLAGS=-i$(TESTOUTDIR)

PATH:=$(MKDIR)cabal-dev/bin:$(PATH)

MD=.make

-include .make/Makefile.srcs

all: utils compiler-bootstrap runtime compiler examples
utils : .make/built-utils
compiler-lib: .make/built-compiler-lib
compiler-bootstrap: .make/built-compiler-bootstrap
runtime: .make/built-runtime
comms-http: .make/comms-http
compiler: .make/built-compiler
examples: .make/built-examples

.make:
	mkdir .make

.make/built-utils: $(UTILS-SRC)
	(cd utils && cabal-dev -s ../cabal-dev install  --force-reinstalls)
	touch .make/built-utils

.make/built-compiler-lib: $(COMPILER-LIB-SRC) .make/built-utils
	(cd compiler-lib && cabal-dev -s ../cabal-dev install  --force-reinstalls)
	touch .make/built-compiler-lib

.make/built-compiler-bootstrap: $(COMPILER-BOOTSTRAP-SRC) .make/built-utils .make/built-compiler-lib
	(cd compiler-bootstrap && cabal-dev -s ../cabal-dev install  --force-reinstalls)
	touch .make/built-compiler-bootstrap

.make/built-runtime: $(RUNTIME-SRC) .make/built-utils .make/built-compiler-bootstrap
	(cd runtime && cabal-dev -s ../cabal-dev install  --force-reinstalls)
	touch .make/built-runtime

.make/comms-http: $(COMMS-HTTP-SRC) .make/built-runtime
	(cd comms-http && cabal-dev -s ../cabal-dev install)
	touch .make/comms-http

.make/built-compiler: $(COMPILER-SRC) .make/built-utils .make/built-runtime
	(cd compiler && cabal-dev -s ../cabal-dev install)
	(cd compiler/tests && ../../cabal-dev/bin/adlc-tests)
	touch .make/built-compiler

.make/built-examples: $(EXAMPLES-SRC) .make/built-utils .make/built-runtime .make/built-compiler .make/comms-http
	(cd examples && cabal-dev -s ../cabal-dev install)
	touch .make/built-examples

boot: .make
	runghc genMake.hs >.make/Makefile.srcs

docs:
	(cd utils && cabal-dev -s ../cabal-dev install --force-reinstalls --enable-documentation)
	(cd runtime && cabal-dev -s ../cabal-dev install --enable-documentation)

clean: 
	-rm -f .make/built-*
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

