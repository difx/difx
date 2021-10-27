AC_INIT([difxcalc], [11.0.3], [David Gordon at NASA Goddard])

AM_INIT_AUTOMAKE([foreign])

AM_SANITY_CHECK

AC_C_BIGENDIAN( \
	[de421_file=DE421_big_Endian], \
	[de421_file=DE421_little_Endian] \
)

AC_PROG_F77
AC_LANG(Fortran)
AC_PROG_FC
AC_FC_FREEFORM

AC_PROG_CC

AC_SEARCH_LIBS([sqrt], [m])
PKG_CHECK_MODULES(GSL, [gsl])

LIBS="${LIBS} ${GSL_LIBS}"
CFLAGS="${CFLAGS} ${GSL_CFLAGS}"

AC_SUBST(de421_file)


AC_CONFIG_FILES([
	Makefile \
	data/Makefile \
	src/Makefile \
	src/param11.i \
])

AC_OUTPUT()
