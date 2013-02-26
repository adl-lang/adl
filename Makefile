ADLDIR=adl
TESTOUTDIR=/tmp/adltest
MODULEPREFIX=ADL.Compiled
ADLCFLAGS=-I $(ADLDIR) -O $(TESTOUTDIR) --moduleprefix=$(MODULEPREFIX)
ADLC=cabal-dev/bin/adlc
ADLLIBDIR=lib
GHC=GHC_PACKAGE_PATH=cabal-dev/packages-7.4.1.conf: ghc
GHCFLAGS=-i$(TESTOUTDIR)

tests:
	$(ADLC) haskell $(ADLCFLAGS) $(ADLDIR)/sys/types.adl
	$(ADLC) haskell $(ADLCFLAGS) $(ADLDIR)/sys/rpc.adl
	$(ADLC) haskell $(ADLCFLAGS) $(ADLDIR)/examples/im.adl
	$(ADLC) haskell $(ADLCFLAGS) $(ADLDIR)/examples/test1.adl
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Sys/Types.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Sys/Rpc.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Examples/Im.hs
	$(GHC) $(GHCFLAGS) $(TESTOUTDIR)/ADL/Compiled/Examples/Test1.hs

tools:
	cabal-dev install

clean: 
	rm -r $(TESTOUTDIR)
