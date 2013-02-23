ADLDIR=adl
TESTOUTDIR=/tmp/adltest
MODULEPREFIX=ADL.Compiled
ADLCFLAGS=-I $(ADLDIR) -O $(TESTOUTDIR) --moduleprefix=$(MODULEPREFIX)
ADLC=GHC_PACKAGE_PATH=cabal-dev/packages-7.4.1.conf: runghc -icompiler compiler/Main.hs
ADLLIBDIR=lib
GHCI=GHC_PACKAGE_PATH=cabal-dev/packages-7.4.1.conf: ghci
GHCIFLAGS=-i$(ADLLIBDIR) -i$(TESTOUTDIR)

test1:
	$(ADLC) haskell $(ADLCFLAGS) $(ADLDIR)/sys/types.adl
	$(ADLC) haskell $(ADLCFLAGS) $(ADLDIR)/sys/rpc.adl
	$(ADLC) haskell $(ADLCFLAGS) $(ADLDIR)/examples/im.adl
	$(GHCI) $(GHCIFLAGS) $(TESTOUTDIR)/ADL/Compiled/Sys/Types.hs </dev/null
	$(GHCI) $(GHCIFLAGS) $(TESTOUTDIR)/ADL/Compiled/Sys/Rpc.hs </dev/null
	$(GHCI) $(GHCIFLAGS) -i$(ADLLIBDIR) $(TESTOUTDIR)/ADL/Compiled/Examples/Im.hs </dev/null

