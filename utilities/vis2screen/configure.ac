AC_INIT(printDiFX.cpp)

PACKAGE=printDiFX
VERSION=1.1

AM_INIT_AUTOMAKE($PACKAGE, $VERSION)

FXCORR_REQUIRED_VERSION=2.9
PKG_CHECK_MODULES(FXCORR, fxcorr >= $FXCORR_REQUIRED_VERSION)
AC_SUBST(FXCORR_CFLAGS)
AC_SUBST(FXCORR_LIBS)

DIFXMESSAGE_REQUIRED_VERSION=2.9
PKG_CHECK_MODULES(DIFXMESSAGE, difxmessage >= $DIFXMESSAGE_REQUIRED_VERSION)
AC_SUBST(DIFXMESSAGE_CFLAGS)
AC_SUBST(DIFXMESSAGE_LIBS)

AC_PROG_CXX

AC_ARG_ENABLE([cuda],
  [AS_HELP_STRING([--enable-cuda], [Enable CUDA acceleration/linking])],
  [enable_cuda=$enableval],
  [enable_cuda=no])

if test "$enable_cuda" = "yes"; then
  AC_MSG_NOTICE([CUDA support enabled. Adding cudart to linker flags.])
  dnl Append the CUDA runtime library to the global LIBS variable.
  dnl The toolkit's lib64 is typically not in the default linker search
  dnl path, so derive it from nvcc's location.
  AC_PATH_PROG([NVCC_FULLPATH],[nvcc])
  if test -n "$NVCC_FULLPATH"; then
    cuda_prefix=`dirname \`dirname "$NVCC_FULLPATH"\``
    if test -d "${cuda_prefix}/lib64"; then
      LIBS="$LIBS -L${cuda_prefix}/lib64 -Wl,-rpath,${cuda_prefix}/lib64"
    fi
  fi
  LIBS="$LIBS -lcudart -lcufft"
fi

AC_OUTPUT(Makefile)
