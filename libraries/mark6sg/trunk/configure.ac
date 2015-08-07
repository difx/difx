AC_INIT([mark6sg], [1.1.5], [Jan Wagner jwagner@kasi.re.kr])

AM_CONFIG_HEADER(config.h)
AC_CONFIG_MACRO_DIR([m4])

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
AC_PROG_CC
AM_PROG_CC_C_O
AM_PROG_LIBTOOL
AM_SANITY_CHECK

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_OUTPUT([ \
	Makefile         \
	mark6sg/Makefile \
	test/Makefile    \
	extra/Makefile   \
	jsmn/Makefile    \
	mark6sg.pc       \
	mark6sg.spec
])
