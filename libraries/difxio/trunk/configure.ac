AC_INIT([difxio],[3.7.0],[Walter Brisken <wbrisken@nrao.edu>])

AC_CONFIG_HEADERS([config.h])

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

AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([foreign])

AC_PROG_CC
LT_INIT
AM_SANITY_CHECK

AC_ARG_WITH(spice-root, [  --with-spice-root=ROOT
                            spice root is ROOT (optional).],
spice_root="$withval", spice_root="$SPICE_ROOT")
if test x"$spice_root" != "x"; then
	CFLAGS="$CFLAGS -I$spice_root/include"
	SPICE_LIBS="$spice_root/lib/cspice.a"
	spice_enabled=true
	echo "Spice enabled"
	AC_DEFINE([HAVE_SPICE], [1], [1=has spice library])
else
	SPICE_LIBS=""
	spice_enabled=false
	echo "Spice disabled"
	AC_DEFINE([HAVE_SPICE], [0], [1=has spice library])
fi
AM_CONDITIONAL(HAVE_SPICE, $spice_enabled)

PKG_CHECK_MODULES(GSL, gsl, [hasgsl=true], [hasgsl=false])
if test "$hasgsl" == "true"; then
	AC_DEFINE([HAVE_GSL], 1, [1=has GSL (Gnu Scientific Library)])
fi
AM_CONDITIONAL(WITH_GSL, $hasgsl)

spicelibs="$SPICE_LIBS"
AC_SUBST(spicelibs)

AC_MSG_CHECKING([whether to enable fftw (default yes)])
AC_ARG_WITH(fftw,
    [  --without-fftw           disable fftw features],
    with_fftw=$withval,
    with_fftw=yes)
AC_MSG_RESULT($with_fftw)

if test "$with_fftw" == "yes"; then
	PKG_CHECK_MODULES(FFTW3, fftw3, [hasfftw=true], [hasfftw=false])
	if test "$hasfftw" == "true"; then
		AC_DEFINE([HAVE_FFTW], 1, [1=has FFTW])
	fi
	AM_CONDITIONAL(WITH_FFTW, $hasfftw)

	LIBS="${LIBS} ${FFTW3_LIBS} ${GSL_LIBS}"
	CFLAGS="${CFLAGS} ${FFTW3_CFLAGS} ${GSL_CFLAGS}"
	AC_SUBST(FFTW3_CFLAGS)
	AC_SUBST(FFTW3_LIBS)
else
	AM_CONDITIONAL(WITH_FFTW, "false")
fi

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_CONFIG_FILES([
	Makefile \
	difxio.pc \
	difxio.spec \
	difxio/Makefile \
	tests/Makefile \
	utils/Makefile \
])
AC_OUTPUT
