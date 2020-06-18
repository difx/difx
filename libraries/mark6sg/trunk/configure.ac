AC_INIT([mark6sg], [2.0.3], [Jan Wagner <jwagner@mpifr.de>, Walter Brisken <wbrisken@nrao.edu>])

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

AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])

OSTYPE=`uname -s`
if [test "x$OSTYPE" != "xDarwin" ] ; then    # Don't run on OSX
  AC_CHECK_LIB(rt, clock_gettime,,[AC_MSG_ERROR("clock_gettime")])
fi

PKG_CHECK_MODULES([FUSE], [fuse], [HAVE_FUSE=1], [HAVE_FUSE=0])
AM_CONDITIONAL([USE_FUSE], [test "$HAVE_FUSE" -eq 1])

AC_CHECK_FUNC([recvmmsg], [HAVE_MMSG=1], [HAVE_MMGS=0])
AM_CONDITIONAL([HAVE_MMSG], [test "$HAVE_MMSG" -eq 1])
if test "$HAVE_MMSG" = 1; then
  AC_DEFINE(HAVE_MMSG, 1, [Define if your OS supports recvmmsg.])
fi

PKG_CHECK_MODULES([DIFXMESSAGE], [difxmessage >= 2.7.0], [HAVE_DIFXMESSAGE=1], [HAVE_DIFXMESSAGE=0])
AM_CONDITIONAL([USE_DIFXMESSAGE], [test "$HAVE_DIFXMESSAGE" -eq 1])
if test "$HAVE_DIFXMESSAGE" = 1; then
  AC_DEFINE(HAVE_DIFXMESSAGE, 1, [Define if you have DiFXMessage library.])
fi

LIBS="${LIBS} ${DIFXMESSAGE_LIBS}"
CFLAGS="${CFLAGS} ${FUSE_CFLAGS} ${DIFXMESSAGE_CFLAGS}"

version=AC_PACKAGE_VERSION
AC_SUBST(version)

AC_OUTPUT([ \
	Makefile         \
	mark6sg/Makefile \
	mark6gather/Makefile \
	test/Makefile    \
	extra/Makefile   \
	jsmn/Makefile    \
	mark6sg.pc       \
	vlbi-mark6sg.spec
])
