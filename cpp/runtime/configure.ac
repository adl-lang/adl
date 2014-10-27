AC_INIT([adlcpp], [0.1], [tim@dockerz.net], [adlcpp], [http://adl.com/])
AC_PREREQ([2.59])
AM_INIT_AUTOMAKE([1.10 -Wall no-define foreign])
AC_CONFIG_HEADERS([config.h adlcppconfig.h])
AC_PROG_CXX
PKG_CHECK_MODULES( [DEPENDENCIES], [cpp-netlib,libb64] )
          
# Turn off shared libraries for now, since they
# make the build process longer and more complex
#
# Also, on OSX missing symbols show up as runtime errors
# rather than linker errors. Why????
LT_INIT([disable-shared])

AC_SUBST([ADLCPP_SO_VERSION], [1:2:0])

# Override the template file name of the generated .pc file, so that there
# is no need to rename the template file when the API version changes.
AC_CONFIG_FILES([Makefile adlcpp.pc:adlcpp.pc.in])
AC_OUTPUT
