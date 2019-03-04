AC_INIT([difx2fits], [3.7.0], [Walter Brisken <wbrisken@nrao.edu>])
AM_CONFIG_HEADER([config.h])

AC_C_BIGENDIAN

AM_INIT_AUTOMAKE([foreign])

AC_PROG_CC
AM_SANITY_CHECK

AC_CHECK_LIB(m, erf,,[AC_MSG_ERROR("need libm")])

PKG_CHECK_MODULES(DIFXIO, difxio >= 3.7.0)

LIBS="${LIBS} ${DIFXIO_LIBS}"
CFLAGS="-D_LARGE_FILE_SOURCE -D_GNU_SOURCE -D_FILE_OFFSET_BITS=64 ${CFLAGS} ${DIFXIO_CFLAGS} "

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

	LIBS="${LIBS} ${FFTW3_LIBS}"
	CFLAGS="${CFLAGS} ${FFTW3_CFLAGS}"
	AC_SUBST(FFTW3_CFLAGS)
	AC_SUBST(FFTW3_LIBS)
else
	AM_CONDITIONAL(WITH_FFTW, "false")
fi



AC_OUTPUT([ \
	Makefile \
	src/Makefile \
	utils/Makefile \
])
