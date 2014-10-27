AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

AM_CPPFLAGS=-I$(top_srcdir)/include -I$(top_srcdir)/src-generated $(DEPENDENCIES_CFLAGS)
AM_CXXFLAGS=-std=c++11 -Wall

lib_LTLIBRARIES = libadlcpp.la

libadlcpp_la_SOURCES = \
    src/adl.cpp src/types.cpp src/sink.cpp src/json.cpp src/jsonimpl.cpp \
    src/comms.cpp src/comms_http.cpp \
    src/example.cpp \
    src-generated/sys.types.cpp src-generated/sys.rpc.cpp \
    src-generated/sys.sinkimpl.cpp src-generated/sys.adlast.cpp

adlcpp_includedir = $(includedir)/adl
adlcpp_libincludedir = $(libdir)/adlcpp/include

adlcpp_include_HEADERS = \
    include/adl/adl.h \
    include/adl/comms.h \
    include/adl/comms_http.h \
    include/adl/json.h \
    include/adl/jsonimpl.h \
    include/adl/sink.h \
    include/adl/types.h \
    src-generated/adl/sys.adlast.h \
    src-generated/adl/sys.rpc.h \
    src-generated/adl/sys.sinkimpl.h \
    src-generated/adl/sys.types.h

TESTS = unit-tests

check_PROGRAMS = unit-tests

unit_tests_SOURCES = src-generated/unittests.cpp \
                     tests/tests-serialisation.cpp \
                     tests/tests-comms.cpp \
                     tests/main.cpp
unit_tests_LDFLAGS=-ladlcpp $(DEPENDENCIES_LIBS)

nodist_adlcpp_libinclude_HEADERS = adlcppconfig.h

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = adlcpp.pc

dist_noinst_SCRIPTS = autogen.sh

