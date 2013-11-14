AC_INIT([m5tone], [2.2.0], [Helge Rottmann <rottmann@mpifr-bonn.mpg.de>])
AM_CONFIG_HEADER(config.h)

AM_INIT_AUTOMAKE

AC_PROG_CC
AM_PROG_CC_C_O
AM_SANITY_CHECK

PKG_CHECK_MODULES(FFTW3, fftw3, [hasfftw=true], [hasfftw=false])
AC_SUBST(FFTW3_CFLAGS)
AC_SUBST(FFTW3_LIBS)
AM_CONDITIONAL(WITH_FFTW, $hasfftw)


AC_OUTPUT([ \
        Makefile \
	])


