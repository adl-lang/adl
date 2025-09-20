#!/usr/bin/env bash
#
set -xe
HERE="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HASKELLDIR=$HERE/../../haskell
ADLSTDLIBDIR=$(cd $HASKELLDIR; stack exec adlc -- show --adlstdlib)
TESTDIR=$HASKELLDIR/compiler/tests

CXXFLAGS="--std=c++17 -I $HERE/../../cpp/runtime/3rd-party -I $HERE/../../cpp/runtime/include -I $HERE/../../cpp/runtime/src-generated/adl -I . -o /tmp/test.o"

# Check that the runtime builds and tests execute
cd $HERE/../../cpp/runtime
rm -rf build && mkdir build
cd build
cmake ..
make
./adl_tests

# Check that the C++ code generated in the tests compiles ok
cd $TESTDIR
g++ -c $CXXFLAGS demo1/cpp-output/picture.cpp
g++ -c $CXXFLAGS test1/cpp-output/test1.cpp
g++ -c $CXXFLAGS test2/cpp-output/test2.cpp
g++ -c $CXXFLAGS test3/cpp-output/test3.cpp
# g++ -c $CXXFLAGS test4/cpp-output/test4.cpp
g++ -c $CXXFLAGS test5/cpp-output/test5.cpp
g++ -c $CXXFLAGS test6/cpp-output-std/sys.adlast.cpp
g++ -c $CXXFLAGS test6/cpp-output-std/sys.dynamic.cpp
g++ -c $CXXFLAGS test6/cpp-output-std/sys.types.cpp
g++ -c $CXXFLAGS test6/cpp-output/test6.cpp
g++ -c $CXXFLAGS test7/cpp-output/test7.cpp
g++ -c $CXXFLAGS test14/cpp-output/test14.cpp
# g++ -c $CXXFLAGS test16/cpp-output/test.cpp
g++ -c $CXXFLAGS test17/cpp-output/test17.cpp
g++ -c $CXXFLAGS test18/cpp-output/test18.cpp
g++ -c $CXXFLAGS test20/cpp-output/test20.cpp
g++ -c $CXXFLAGS test29/cpp-output/test29.cpp
