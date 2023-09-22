#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([datasim], [1.0], [zheng.meyer@gmail.com])
AM_INIT_AUTOMAKE
AC_CONFIG_SRCDIR([src/util.cpp])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
LT_INIT
AC_PROG_CXX([mpic++ mpicxx])
AC_PROG_CC([mpicc])
AC_PROG_MAKE_SET

# Checks for libraries.
AC_CHECK_LIB([gsl], [gsl_ran_gaussian_ziggurat])
AC_CHECK_LIB([gslcblas], [cblasdsdot])
AC_CHECK_LIB([mpifxcorr], [calculateDelayInterpolator])

# Use C++11
#AX_CXX_COMPILE_STDCXX_0X([ext],[mandatory])

PKG_CHECK_MODULES(VDIFIO, vdifio >= 1.0)
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.4.0 difxmessage < 7])
PKG_CHECK_MODULES(MARK5ACCESS, mark5access >= 1.5.1)
m5ipc_enabled=true
PKG_CHECK_MODULES(MARK5IPC,
        [mark5ipc >= 2.4.0 mark5ipc < 7],
        [AC_DEFINE([HAVE_MARK5IPC], 1, [1=has MARK5IPC])],
        m5ipc_enabled=false)
PKG_CHECK_MODULES(MPIFXCORR, mpifxcorr >= 2.4.0)

# Checks for header files.
AC_CHECK_HEADERS([unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
#AC_CHECK_HEADER_STDBOOL
AC_C_INLINE
AC_TYPE_SIZE_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_CHECK_FUNCS([floor pow rint sqrt])

LIBS="${LIBS} ${DIFXIO_LIBS} ${MPIFXCORR_LIBS} ${DIFXMESSAGE_LIBS} ${MARK5ACCESS_LIBS} ${MARK5IPC_LIBS}"
CFLAGS="${CFLAGS} ${DIFXIO_CFLAGS} ${MPIFXCORR_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${MARK5ACCESS_CFLAGS} ${MARK5IPC_LIBS}"
CXXFLAGS="${CFLAGS}"

AC_CONFIG_FILES([Makefile
                 src/Makefile
                 scripts/Makefile
		 examples/Makefile])
AC_OUTPUT
