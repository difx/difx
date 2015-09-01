AC_INIT([mpifxcorr], [2.5], [Adam Deller <adeller@astro.swin.edu.au>])
PACKAGE_VERSION=AC_PACKAGE_VERSION
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([LIBRARY_VERSION])
AM_INIT_AUTOMAKE
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
PKG_CHECK_MODULES(DIFXMESSAGE, [difxmessage >= 2.5.0 difxmessage < 7])
CPPFLAGS="${CPPFLAGS} ${DIFXMESSAGE_CFLAGS}"

dnl **********************************************************
dnl **** Find vdifio library                              ****
dnl **********************************************************
PKG_CHECK_MODULES(VDIFIO, vdifio >= 1.1)
CPPFLAGS="${CPPFLAGS} ${VDIFIO_CFLAGS}"

dnl **********************************************************
dnl **** Find mark5ipc library                            ****
dnl **********************************************************
m5ipc_enabled=true
PKG_CHECK_MODULES(MARK5IPC,
        [mark5ipc >= 2.4.0 mark5ipc < 7],
	[AC_DEFINE([HAVE_MARK5IPC], 1, [1=has MARK5IPC])],
	m5ipc_enabled=false)
AM_CONDITIONAL(WITH_MARK5IPC, $m5ipc_enabled)

dnl **********************************************************
dnl **** Look for StreamStor libraries                    ****
dnl **** cause native Mark5 to be built if found.         ****
dnl **********************************************************
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
AM_CONDITIONAL(WITH_SS, $ss_enabled)

dnl **********************************************************
dnl **** Look for Mark6 scatter/gather library            ****
dnl **** also require vdifmark6sg.h, implying vdifio      ****
dnl **** was compiled with mark6sg support.               ****
dnl **** cause mark6 datastreams to be built if found     ****
dnl **********************************************************
mark6sg_enabled=true
PKG_CHECK_EXISTS([mark6sg >= 1.1.6],
	[ PKG_CHECK_MODULES(MARK6SG, mark6sg) ],
	[ AC_MSG_NOTICE([Did not find mark6sg.pc in $PKG_CONFIG_PATH])
	  mark6sg_enabled=false ])
if test ${mark6sg_enabled} = true; then
	CPPFLAGS="${CPPFLAGS} ${MARK6SG_CFLAGS}"
	AC_CHECK_HEADERS([mark6sg/mark6_sg_vfs.h], [], [ mark6sg_enabled=false ])
	AC_CHECK_HEADERS([vdifmark6sg.h], [], [ mark6sg_enabled=false ])
fi
if test ${mark6sg_enabled} = true; then
	CPPFLAGS="${CPPFLAGS} ${MARK6SG_CFLAGS}"
	CXXFLAGS="${CXXFLAGS} ${MARK6SG_CFLAGS}"
else
	MARK6SG_CFLAGS=""
	MARK6SG_LIBS=""
	echo ''
	echo 'Mark6 support will be suppressed because one or more'
	echo 'needed files was not found.  Mark6 support requires'
	echo 'the mark6sg library to be installed _and_ the vdifio'
	echo 'library to be installed with support for mark6sg.'
	echo ''
fi
AM_CONDITIONAL(WITH_MARK6SG, $mark6sg_enabled)

dnl **********************************************************
dnl **** Find mark5access library                         ****
dnl **********************************************************
PKG_CHECK_MODULES(M5ACCESS, mark5access >= 1.5.3)
dnl **********************************************************

dnl for Mutex lock in datastream.cpp
AC_CHECK_LIB(rt, clock_gettime)

CXXFLAGS="${CXXFLAGS} ${M5ACCESS_CFLAGS} ${VDIFIO_CFLAGS} ${MATH_CFLAGS} ${FFTW3_CFLAGS} ${DIFXMESSAGE_CFLAGS} ${MARK5IPC_CFLAGS} ${CFLAG_QUIET} ${OPENMP_CXXFLAGS}"
LIBS="${M5ACCESS_LIBS} ${VDIFIO_LIBS} ${SS_LIBS} ${MATH_LIBS} ${DIFXMESSAGE_LIBS} ${MARK5IPC_LIBS} $LIBS"

echo "CXXFLAGS = ${CXXFLAGS}"
echo "LIBS = ${LIBS}"
AC_OUTPUT([
	Makefile \
	mpifxcorr.pc \
	fxcorr.pc \
	src/Makefile \
	src/architecture.h \
	scripts/Makefile \
	utils/Makefile\
])
