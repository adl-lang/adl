MKFILE:=$(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
MKDIR:=$(dir $(MKFILE))
SANDBOX=.cabal-sandbox
SANDBOXBIN=$(MKDIR)$(SANDBOX)/bin

TESTOUTDIR=/tmp/adltest
MODULEPREFIX=ADL.Compiled
ADLCFLAGS=-O $(TESTOUTDIR) --moduleprefix=$(MODULEPREFIX)
ADLC=$(SANDBOXBIN)/adlc

PATH:=$(SANDBOXBIN):$(PATH)

-include .make/Makefile.srcs

all: utils compiler-bootstrap runtime compiler runtime-cpp examples
utils : .make/built-utils
compiler-lib: .make/built-compiler-lib
compiler-bootstrap: .make/built-compiler-bootstrap
runtime: .make/built-runtime
runtime-cpp: .make/built-runtime-cpp
comms-http: .make/comms-http
compiler: .make/built-compiler
examples: .make/built-examples

.make:
	mkdir -p .make

$(SANDBOX):
	cabal sandbox init

depends: $(SANDBOX) .make
	runghc genMake.hs >.make/Makefile.srcs

.make/built-utils: $(UTILS-SRC)
	cabal install --force-reinstalls utils/
	touch .make/built-utils

.make/built-compiler-lib: $(COMPILER-LIB-SRC) .make/built-utils
	cabal install --force-reinstalls compiler-lib/
	touch .make/built-compiler-lib

.make/built-compiler-bootstrap: $(COMPILER-BOOTSTRAP-SRC) .make/built-utils .make/built-compiler-lib
	cabal install --force-reinstalls compiler-bootstrap/
	touch .make/built-compiler-bootstrap

.make/built-runtime: $(RUNTIME-SRC) .make/built-utils .make/built-compiler-bootstrap
	cabal install --force-reinstalls runtime/
	touch .make/built-runtime

.make/comms-http: $(COMMS-HTTP-SRC) .make/built-runtime
	cabal install --force-reinstalls comms-http/
	touch .make/comms-http

.make/built-compiler: $(COMPILER-SRC) .make/built-utils .make/built-runtime
	cabal install --force-reinstalls compiler/
	(cd compiler/tests && $(SANDBOXBIN)/adlc-tests)
	touch .make/built-compiler

.make/built-runtime-cpp: $(RUNTIME-SRC) $(RUNTIME-CPP-SRC) .make/built-compiler
	(cd runtime-cpp && make clean)
	(cd runtime-cpp && make)
	touch .make/built-runtime-cpp

.make/built-examples: $(EXAMPLE-SRC) .make/built-utils .make/built-runtime .make/built-compiler .make/comms-http
	cabal install --force-reinstalls examples/
	touch .make/built-examples

docs:
	cabal install utils/ --force-reinstalls --enable-documentation
	cabal install runtime/ --force-reinstalls --enable-documentation
	cabal install comms-http/ --force-reinstalls --enable-documentation

clean: 
	-rm -f .make/built-*
	-cabal sandbox hc-pkg unregister adl-compiler-lib
	-cabal sandbox hc-pkg unregister adl-comms-http
	-cabal sandbox hc-pkg unregister adl-runtime
	-cabal sandbox hc-pkg unregister adl-utils

	-(cd examples; cabal clean ; rm -rf dist)
	-(cd compiler; cabal clean ; rm -rf dist)
	-(cd compiler-bootstrap; cabal clean ; rm -rf dist)
	-(cd compiler-lib ; cabal clean ; rm -rf dist)
	-(cd utils ; cabal clean ; rm -rf dist)
	-(cd runtime ; cabal clean ; rm -rf dist)
	-(cd runtime-cpp ; make clean)

cleanext:
	-cabal sandbox hc-pkg unregister zeromq3-haskell
	(cd ext && rm -rf include lib share)

ext: zeromq3-haskell

zeromq3-haskell: ext/lib/libzmq.so.3.0.0
	 cabal install zeromq3-haskell --extra-include-dirs=$(MKDIR)ext/include --extra-lib-dirs=$(MKDIR)/ext/lib --enable-documentation

ext/lib/libzmq.so.3.0.0: ext/downloads/zeromq-3.2.2.tar.gz
	(cd ext && tar -xzf downloads/zeromq-3.2.2.tar.gz)
	(cd ext/zeromq-3.2.2 && ./configure --prefix $(MKDIR)ext)
	(cd ext/zeromq-3.2.2 && make)
	(cd ext/zeromq-3.2.2 && make install)
	rm -r ext/zeromq-3.2.2

ext/downloads/zeromq-3.2.2.tar.gz:
	mkdir -p ext/downloads
	(cd ext/downloads && wget http://download.zeromq.org/zeromq-3.2.2.tar.gz)

# ghc-mod want's to find the cabal.sandbox.config file in the 
# same directory as the cabal file. We only have one config for 
# the whole tree. Create some symlinks to sort this out.
sandbox-links:
	(cd utils && ln -fs ../cabal.sandbox.config)
	(cd compiler-lib && ln -fs ../cabal.sandbox.config)
	(cd compiler-bootstrap && ln -fs ../cabal.sandbox.config)
	(cd runtime && ln -fs ../cabal.sandbox.config)
	(cd comms-http && ln -fs ../cabal.sandbox.config)
	(cd examples && ln -fs ../cabal.sandbox.config)

.PHONY : examples utils compiler-bootstrap runtime compiler clean

