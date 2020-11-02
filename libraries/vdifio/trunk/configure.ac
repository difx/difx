AC_INIT([VDIFIO], [1.5], [difx-developers@nrao.edu])
#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.50)
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([src/vdifio.h])
AM_CONFIG_HEADER([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CHECK_SIZEOF([size_t])
AC_SYS_LARGEFILE
AX_OPENMP

AC_C_BIGENDIAN

# Checks for programs.
AC_PATH_PROG([PYTHON],[python])
AC_PROG_CC
AC_PROG_LIBTOOL

# Checks for libraries.
AC_CHECK_LIB(m, sqrt)
AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

# Checks for conditional builds

AC_MSG_CHECKING([whether to build Python bindings])
AC_ARG_ENABLE([python],
        AS_HELP_STRING([--enable-python],[enable Python bindings (ctypes) for mark5access]),
        [case "${enableval}" in
           yes | no ) REQUESTED_PYTHON_CTYPES="${enableval}" ;;
           *        ) AC_MSG_ERROR(bad value ${enableval} for --enable-python) ;;
         esac],[REQUESTED_PYTHON_CTYPES="no"]
)
AM_CONDITIONAL([REQUESTED_PYTHON_CTYPES], [test "x$REQUESTED_PYTHON_CTYPES" = "xyes"])
AC_MSG_RESULT($REQUESTED_PYTHON_CTYPES)

if test -n "$PYTHON" -a "x$REQUESTED_PYTHON_CTYPES" = "xyes"; then
        AC_MSG_CHECKING([whether required Python modules ctypes, ctypeslib are installed])
        ac_ctypes_result=`$PYTHON -c "import ctypes, ctypeslib" 2>&1`
        if test -z "$ac_ctypes_result"; then
                AC_MSG_RESULT([yes])
                GOT_PYTHON_CTYPES="yes"
        else
                AC_MSG_RESULT([no])
                AC_MSG_WARN([cannot import Python module "ctypes". Please check your Python installation.])
        fi
fi
AM_CONDITIONAL([ENABLE_PYTHON_CTYPES], [test -n "$PYTHON" -a "x$REQUESTED_PYTHON_CTYPES" = "xyes" -a -n "$GOT_PYTHON_CTYPES"])

PKG_CHECK_MODULES([MARK6SG], [mark6sg >= 2.0.2], [HAVE_MARK6SG=true], [HAVE_MARK6SG=false])
AM_CONDITIONAL(USE_MARK6SG, $HAVE_MARK6SG)
if test ${HAVE_MARK6SG} = true; then
  AC_DEFINE(HAVE_MARK6SG, 1, [Define if you have mark6sg library.])
  CFLAGS="${CFLAGS} ${MARK6SG_CFLAGS}"
  LIBS="${LIBS} ${MARK6SG_LIBS}"
  AC_SUBST(MARK6SG_LIBS)
fi


dnl AC_ARG_WITH([generateVDIF],
dnl     AS_HELP_STRING([--with-generateVDIF], [Enable generate VDIF]))
dnl if test "x${with_generateVDIF}" == "xyes"; then
dnl     AM_CONDITIONAL([WITH_GENERATEVDIF], [true])
dnl else
dnl     AM_CONDITIONAL([WITH_GENERATEVDIF], [false])
dnl fi

dnl **********************************************************
dnl **** Set path for Intel Performance Primitive library ****
dnl **********************************************************

ipp_enabled=1
AC_ARG_ENABLE(ipp, [AS_HELP_STRING([--disable-ipp],[disables IPP acceleration])])
if test "x$enable_ipp" = xno; then
        ipp_enabled=0
fi
PKG_CHECK_MODULES([IPP], ipp, [AC_DEFINE([HAVE_IPP], 1, [1=has IPP])], ipp_enabled=0)
AM_CONDITIONAL([USE_IPP], [test "$ipp_enabled" -eq 1])
AS_IF([test "$ipp_enabled" -eq 1], [AC_DEFINE([USE_IPP], [1], [Define if using ipp.])])
AC_SUBST(ipp_enabled)
dnl **********************************************************

if test ${ipp_enabled} = 1; then
        FFT_LIBS="${IPP_LIBS} ${LIBS}"
        CFLAGS="${CFLAGS} ${IPP_CFLAGS}"
fi

# Add codif support to a handful of the utilities
PKG_CHECK_MODULES(CODIFIO, codifio, [HAVE_CODIFIO=1],  [HAVE_CODIFIO=0])
AM_CONDITIONAL([USE_CODIFIO], [test "$HAVE_CODIFIO" -eq 1])
AS_IF([test "$HAVE_CODIFIO" -eq 1], [AC_DEFINE([USE_CODIFIO], [1], [Define if using codifio.])])
if test ${HAVE_CODIFIO} = 1; then
      	CFLAGS="${CFLAGS} ${CODIFIO_CFLAGS}"
fi

# Prepare output files

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_OUTPUT([ \
	Makefile \
	vlbi-vdifio.spec \
	src/Makefile \
	utils/Makefile \
	python/Makefile \
	vdifio.pc \
])
