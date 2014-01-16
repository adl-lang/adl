# You may need to override these from the make command line:
#     ADLC - the adl compiler
#     ADLDIR - the directory containing the ADL standard library

ADLC= adlc
ADLDIR=.
ADLFLAGS = --include-prefix=adl -I $(ADLDIR)

all:
	$(ADLC) cpp $(ADLFLAGS) $(ADLDIR)/sys/types.adl
	$(ADLC) cpp $(ADLFLAGS) $(ADLDIR)/sys/rpc.adl
	$(ADLC) cpp $(ADLFLAGS) $(ADLDIR)/sys/sinkimpl.adl
	$(ADLC) cpp $(ADLFLAGS) $(ADLDIR)/sys/adlast.adl
	$(ADLC) cpp $(ADLFLAGS) unittests.adl
