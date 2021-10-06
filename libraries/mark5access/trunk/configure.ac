AC_INIT([mark5access], [1.6], [Walter Brisken <wbrisken@nrao.edu>])

AC_CONFIG_HEADERS(config.h)

AC_C_BIGENDIAN

#shared library versioning
LIBRARY_VERSION=0:0:0
#               | | |
#        +------+ | +---+
#        |        |     |
#     current:revision:age
#        |        |     |
#        |        |     +- increment if interfaces have been added
#        |        |        set to zero if interfaces have been removed
#                          or changed
#        |        +- increment if source code has changed
#        |           set to zero if current is incremented
#        +- increment if interfaces have been added, removed or changed

PACKAGE_VERSION=AC_PACKAGE_VERSION

AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])

AM_INIT_AUTOMAKE([foreign])

AC_CONFIG_MACRO_DIR([m4])

AC_SYS_LARGEFILE
AC_PATH_PROG([PYTHON],[python])
AC_PROG_CC
AM_PROG_CC_C_O
LT_INIT
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])
PKG_CHECK_MODULES(FFTW3, fftw3, [hasfftw=true], [hasfftw=false])

AC_SUBST(FFTW3_CFLAGS)
AC_SUBST(FFTW3_LIBS)
AM_CONDITIONAL(WITH_FFTW, $hasfftw)

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

PKG_CHECK_MODULES([MARK6SG], [mark6sg >= 2.0.1], [HAVE_MARK6SG=true], [HAVE_MARK6SG=false])
AM_CONDITIONAL(USE_MARK6SG, $HAVE_MARK6SG)
if test ${HAVE_MARK6SG} = true; then
  AC_DEFINE(HAVE_MARK6SG, 1, [Define if you have mark6sg library.])
  CFLAGS="${CFLAGS} ${MARK6SG_CFLAGS}"
  LIBS="${LIBS} ${MARK6SG_LIBS}"
  AC_SUBST(MARK6SG_LIBS)
fi

PKG_CHECK_MODULES([CODIFIO], [codifio >= 0.2], [HAVE_CODIFIO=true], [HAVE_CODIFIO=false])
AM_CONDITIONAL(USE_CODIFIO, $HAVE_CODIFIO)
if test ${HAVE_CODIFIO} = true; then
  AC_DEFINE(HAVE_CODIFIO, 1, [Define if you have codifio library.])
  CFLAGS="${CFLAGS} ${CODIFIO_CFLAGS}"
  LIBS="${LIBS} ${CODIFIO_LIBS}"
# AC_SUBST(CODIFIO_LIBS)
fi

PKG_CHECK_MODULES([MARK6SG], [mark6sg >= 2.0.2], [hasmark6sg=true], [hasmark6sg=false])
AM_CONDITIONAL(WITH_MARK6SG, $hasmark6sg)
if test $hasmark6sg = true; then
  CPPFLAGS="${CPPFLAGS} ${MARK6SG_CFLAGS}"
  AC_DEFINE([HAVE_MARK6SG], 1, [1=has MARK5SG])
  AC_DEFINE(hasmark6sg, 1, [Define if you have mark6sg library.])
fi


version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_CONFIG_FILES([ \
	Makefile \
	mark5access.pc \
	vlbi-mark5access.spec \
	mark5access/Makefile \
	bbsum/Makefile \
	examples/Makefile \
	doc/Makefile \
        python/Makefile \
])
AC_OUTPUT

