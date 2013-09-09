ADLC=../cabal-dev/bin/adlc
ADLDIR=../runtime/adl/sys
CPPFLAGS=-Ibuild -Iinclude
CXXFLAGS=-std=c++11 -fpic

all:
	mkdir -p build
	$(ADLC) cpp -O build $(ADLDIR)/types.adl
	$(ADLC) cpp -O build $(ADLDIR)/rpc.adl
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/sys.types.o build/sys.types.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/sys.rpc.o build/sys.rpc.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/adl.o src/adl.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/types.o src/types.cpp
	g++ -c $(CPPFLAGS) $(CXXFLAGS) -o build/json.o src/json.cpp
	g++ -shared -o build/libadl.so build/*.o

clean:
	rm -rf build

