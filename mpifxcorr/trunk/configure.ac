AC_INIT([mpifxcorr], [2.7], [Adam Deller <adeller@astro.swin.edu.au>])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE([subdir-objects])
AM_CONFIG_HEADER(config.h)
AC_LANG_CPLUSPLUS
AC_PROG_CXX
AX_OPENMP
AC_PROG_RANLIB
AC_C_BIGENDIAN

AC_LANG(C++)

AC_ARG_ENABLE(quiet, [AS_HELP_STRING([--enable-quiet],[disable some verbiage])], [CFLAG_QUIET="-DQUIET"], [CFLAG_QUIET=" "])

dnl **********************************************************
dnl **** Set path for Intel Performance Primitive library ****
dnl **********************************************************

ipp_enabled=1
AC_ARG_ENABLE(ipp, [AS_HELP_STRING([--disable-ipp],[disables IPP acceleration])])
if test "x$enable_ipp" = xno; then
	ipp_enabled=0
fi
PKG_CHECK_MODULES(IPP, ipp, [AC_DEFINE([HAVE_IPP], 1, [1=has IPP])], ipp_enabled=0)
AC_SUBST(ipp_enabled)
dnl **********************************************************

AC_CHECK_LIB(pthread, pthread_create,,[AC_MSG_ERROR("need libpthread")])

if test ${ipp_enabled} = 0; then
	PKG_CHECK_MODULES(FFTW3, fftw3, [hasfftw=true], [hasfftw=false])
	if test "$hasfftw" == "false"; then
		AC_MSG_ERROR([Error: fftw needs to be installed if IPP is not])
	fi
	PKG_CHECK_MODULES(FFTW3F, fftw3f, [hasfftwf=true], [hasfftwf=false])
	if test "$hasfftwf" == "false"; then
		AC_MSG_ERROR([Error: fftwf needs to be installed if IPP is not])
	fi
	MATH_LIBS="${FFTW3_LIBS} ${FFTW3F_LIBS}"
	MATH_CFLAGS="${FFTW3_CFLAGS} ${FFTW3F_CFLAGS}"
else
	MATH_LIBS="${IPP_LIBS} ${LIBS}"
	MATH_CFLAGS="${IPP_CFLAGS}"
fi

AC_SUBST(mathlibs, [${MATH_LIBS}])
AC_SUBST(mathcflags, [${MATH_CFLAGS}])

dnl **********************************************************
dnl **** Find difxmessage library                         ****
dnl **********************************************************
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.7.0])
CPPFLAGS="${CPPFLAGS} ${DIFXMESSAGE_CFLAGS}"

dnl **********************************************************
dnl **** Find vdifio library                              ****
dnl **********************************************************
PKG_CHECK_MODULES(VDIFIO, vdifio >= 1.5)
CPPFLAGS="${CPPFLAGS} ${VDIFIO_CFLAGS}"

dnl **********************************************************
dnl **** Find mark6sg library                              ****
dnl **********************************************************
PKG_CHECK_MODULES([MARK6SG], [mark6sg >= 2.0.2], [hasmark6sg=true], [hasmark6sg=false])
AM_CONDITIONAL(WITH_MARK6SG, $hasmark6sg)
if test $hasmark6sg = true; then
  CPPFLAGS="${CPPFLAGS} ${MARK6SG_CFLAGS}"
  AC_DEFINE([HAVE_MARK6SG], 1, [1=has MARK5SG])
  AC_DEFINE(hasmark6sg, 1, [Define if you have mark6sg library.])
fi


dnl **********************************************************
dnl **** Find mark5ipc library                            ****
dnl **********************************************************
m5ipc_enabled=true
PKG_CHECK_MODULES(MARK5IPC,
        [mark5ipc >= 2.7.0],
	[AC_DEFINE([HAVE_MARK5IPC], 1, [1=has MARK5IPC])],
	m5ipc_enabled=false)
AM_CONDITIONAL(WITH_MARK5IPC, $m5ipc_enabled)

dnl **********************************************************
dnl **** Look for dirlist library                         ****
dnl **** Now required for native mark5 and mark6          ****
dnl **********************************************************
has_dirlist=true
PKG_CHECK_EXISTS([dirlist >= 0.1],
	[ PKG_CHECK_MODULES(DIRLIST, dirlist) ],
	[ AC_MSG_NOTICE([Did not find dirlist in $PKG_CONFIG_PATH])
	  has_dirlist=false])
if test ${has_dirlist=true} = true; then
	CPPFLAGS="${CPPFLAGS} ${DIRLIST_CFLAGS}"
	CXXFLAGS="${CXXFLAGS} ${DIRLIST_CFLAGS}"
else
	DIRLIST_CFLAGS=""
	DIRLIST_LIBS=""
	echo ''
	echo 'No suitable dirlist.pc file was found so native'
	echo 'Mark5 and Mark6 support will not be attempted.'
	echo ''
fi
AM_CONDITIONAL(HAS_DIRLIST, $has_dirlist)


AC_SUBST(difxlibs, [${DIRLIST_LIBS} ${M5ACCESS_LIBS}])
AC_SUBST(difxcflags, [${DIRLIST_CFLAGS} ${M5ACCESS_CFLAGS}])

dnl **********************************************************
dnl **** Look for StreamStor libraries                    ****
dnl **** cause native Mark5 to be built if found.         ****
dnl **********************************************************
if test ${has_dirlist} = true; then
	ss_enabled=true
	PKG_CHECK_EXISTS([streamstor >= 8.0],
		[ PKG_CHECK_MODULES(SS, streamstor) ],
		[ AC_MSG_NOTICE([Did not find STREAMSTOR in $PKG_CONFIG_PATH])
		  ss_enabled=false])
	if test ${ss_enabled} = true; then
		CPPFLAGS="${CPPFLAGS} ${SS_CFLAGS}"
		CXXFLAGS="${CXXFLAGS} ${SS_CFLAGS}"
		AC_CHECK_HEADERS([xlrapi.h])
	else
		SS_CFLAGS=""
		SS_LIBS=""
		echo ''
		echo 'No suitable streamstor.pc file was found so the native'
		echo 'Mark5 support will be suppressed.'
		echo ''
	fi
else
	ss_enabled=false
fi
AM_CONDITIONAL(WITH_SS, $ss_enabled)

dnl **********************************************************
dnl **** Find mark5access library                         ****
dnl **********************************************************
PKG_CHECK_MODULES(M5ACCESS, mark5access >= 1.5.4)


dnl for Mutex lock in datastream.cpp
AC_CHECK_LIB(rt, clock_gettime)

CXXFLAGS="${CXXFLAGS} ${M5ACCESS_CFLAGS} ${VDIFIO_CFLAGS} ${MARK6SG_CFLAGS} ${MATH_CFLAGS} ${FFTW3_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${MARK5IPC_CFLAGS} ${CFLAG_QUIET} ${OPENMP_CXXFLAGS} ${DIRLIST_CFLAGS} ${MARK6SG_CFLAGS}"
LIBS="${M5ACCESS_LIBS} ${VDIFIO_LIBS} ${MARK6SG_LIBS} ${SS_LIBS} ${MATH_LIBS} ${DIFXMESSAGE_LIBS} ${MARK5IPC_LIBS} ${DIRLIST_LIBS} $LIBS"

echo "CXXFLAGS = ${CXXFLAGS}"
echo "LIBS = ${LIBS}"
AC_OUTPUT([
	Makefile \
	mpifxcorr.pc \
	fxcorr.pc \
	src/Makefile \
	src/architecture.h \
	utils/Makefile\
])
