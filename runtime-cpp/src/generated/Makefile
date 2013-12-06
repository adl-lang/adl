ADLBASE = ../../..
ADLC=$(ADLBASE)/.cabal-sandbox/bin/adlc
ADLDIR=$(ADLBASE)/runtime/adl

all:
	$(ADLC) cpp $(ADLDIR)/sys/types.adl
	$(ADLC) cpp $(ADLDIR)/sys/rpc.adl
	$(ADLC) cpp $(ADLDIR)/sys/sinkimpl.adl
	$(ADLC) cpp -I $(ADLDIR) $(ADLDIR)/sys/adlast.adl
