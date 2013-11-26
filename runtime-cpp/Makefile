ADLC=../.cabal-sandbox/bin/adlc
ADLDIR=../runtime/adl
CPPFLAGS=-Ibuild -Iinclude
CXXFLAGS=-std=c++11 -fpic

all:
	mkdir -p build
	$(ADLC) cpp -O build $(ADLDIR)/sys/types.adl
	$(ADLC) cpp -O build $(ADLDIR)/sys/rpc.adl
	$(ADLC) cpp -O build $(ADLDIR)/sys/sinkimpl.adl
	$(ADLC) cpp -O build -I $(ADLDIR) $(ADLDIR)/sys/adlast.adl
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/json.o src/json.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/adl.o src/adl.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/types.o src/types.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/sink.o src/sink.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/comms.o src/comms.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/comms_http.o src/comms_http.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/sys.types.o build/sys.types.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/sys.rpc.o build/sys.rpc.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/sinkimpl.o build/sys.sinkimpl.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/adlast.o build/sys.adlast.cpp
	g++ -shared -o build/libadl.so build/*.o

clean:
	rm -rf build

