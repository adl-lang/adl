MKFILE:=$(CURDIR)/$(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
MKDIR:=$(dir $(MKFILE))

TESTOUTDIR=/tmp/adltest
MODULEPREFIX=ADL.Compiled
ADLCFLAGS=-O $(TESTOUTDIR) --moduleprefix=$(MODULEPREFIX)

-include .make/Makefile.srcs

dist: allhaskell
	(cd haskell && tools/make-dist.hs)

allhaskell:
	(cd haskell && stack build ./compiler-bootstrap)
	(cd haskell && stack build ./compiler)
	(cd haskell && stack build)


runtime-cpp: .make/built-runtime-cpp

.make:
	mkdir -p .make

depends: .make
	stack runghc mkdepends.hs >.make/Makefile.srcs

.make/built-runtime-cpp: $(RUNTIME-CPP-SRC) allhaskell
	(cd cpp/runtime && ./autogen.sh)
	(cd cpp/runtime && mkdir -p build && cd build && ../configure)
	(cd cpp/runtime/build && make)
	(cd cpp/runtime/build && make check)
	touch .make/built-runtime-cpp

docs:
	cd haskell && stack haddock

clean: 
	-rm -f .make/built-*
	-(cd haskelll stack clean)
	-(cd cpp/runtime ; rm -rf build)

cleanext:
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

.PHONY : all allhaskell runtime-cpp

