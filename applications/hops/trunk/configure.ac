#
# Process this file with autoconf to produce a configure script.
# Run autoscan to get suggestions on more things to check.
#
# Note that HOPS doesn't presently use hops_config.h except
# to detect endianism in bytflp.h for some of the tools.
#
AC_PREREQ(2.59)
AC_INIT(hops, 3.23, gbc@haystack.mit.edu)
AM_INIT_AUTOMAKE([foreign -Wno-extra-portability subdir-objects])

AC_CONFIG_SRCDIR([hops.bash.in])
AC_CONFIG_HEADERS([hops_config.h])
AC_CONFIG_MACRO_DIR([m4])
m4_ifset([LT_INIT], [LT_INIT], [AC_PROG_LIBTOOL])
m4_ifset([LT_WITH_LTDL], [LT_WITH_LTDL], [AC_LIB_LTDL])

AC_PROG_CC

# ignore this line
AC_GBC_INIT([This file captures critical configuration details])
GBC_HOPS_PRINTER()

#
# For configure testing, set ac_hops=false to speed things up
#
if [test -z "$ac_hops"]; then ac_hops=true; else ac_hops=false; fi
PKG_PROG_PKG_CONFIG([0.0])

#
# See what autoconf thinks we are working with.
#
AC_CANONICAL_BUILD
AC_GBC_NOTICE([cpu: $build_cpu vendor: $build_vendor os: $build_os])

#
# Precious variables which affect the universe
#
AC_ARG_VAR(HOPS_ROOT, [Root directory for the HOPS tools installation])

#
# Allows --prefix to be skipped and HOPS_ROOT to be deduced.
#
AC_PREFIX_DEFAULT([${HOPS_ROOT-'$HOPS_ROOT'}/`uname -m`-${VERSION-'$VERSION'}])

#
# HOPS_ROOT is deduced if not predefined
#
if test -z "$HOPS_ROOT" ; then
    AC_GBC_NOTICE([One: prefix is $prefix])
    if test "$prefix" = NONE ; then
        AC_GBC_NOTICE([no --prefix, setting HOPS_ROOT to parent of cwd])
        prefix=`pwd`
        # prefix=`AS_DIRNAME($prefix)`
        prefix=`AS_DIRNAME($prefix)`/`uname -m`-$VERSION
    fi
    AC_GBC_NOTICE([Two: prefix is $prefix])
    HOPS_ROOT=`AS_DIRNAME($prefix)`
    AC_GBC_NOTICE([Setting HOPS_ROOT to $HOPS_ROOT])
else
    if test "$prefix" = /`uname -m` -o "$prefix" = NONE ; then
        prefix=${HOPS_ROOT}/`uname -m`-$VERSION
        AC_GBC_NOTICE([Set: prefix is $prefix])
    fi
fi
HOPS_ARCH=`uname -m`-$VERSION
AC_GBC_NOTICE([Setting HOPS_ARCH to $HOPS_ARCH])
AC_SUBST(HOPS_ARCH,$HOPS_ARCH)

#
# Collected top-level enable/disable
# AC_ARG_ENABLE(FEATURE, HELP-STRING, [ACTION-IF-GIVEN], [ACTION-IF-NOT-GIVEN])
# ACTION-IF-GIVEN refers to any of these:
#   --enable-FEATURE=yes or
#   --enable-FEATURE -> enableval=$enable_FEATURE and enable_FEATURE=yes
#   --disable-FEATURE -> enableval=$enable_FEATURE and enable_FEATURE=no
#   --disable-FEATURE=anything turns into enable_FEATURE=no
#
AC_ARG_ENABLE(hops,
    AS_HELP_STRING([--disable-hops], [disables the post-processing tools]),
    [], [enable_hops=yes])
AC_GBC_NOTICE([enable_hops is --$enable_hops--])
AC_ARG_ENABLE(vex2xml,
    AS_HELP_STRING([--enable-vex2xml], [enable VEX2XML tool]),
    [], [enable_vex2xml=yes])
AC_GBC_NOTICE([enable_vex2xml is --$enable_vex2xml--])
AC_ARG_ENABLE(antlr4,
    AS_HELP_STRING([--enable-antlr4], [enable Antlr4 used by vex2xml]),
    [], [enable_antlr4=no])
AC_GBC_NOTICE([enable_antlr4 is --$enable_antlr4--])
###
AC_ARG_ENABLE(chops,
    AS_HELP_STRING([--disable-chops], [disables the vgos-processing tools]),
    [], [enable_chops=yes])
AC_GBC_NOTICE([enable_chops is --$enable_chops--])
# this one is obsolete and confusing, so die and explain.
AC_ARG_ENABLE(hopes,
    AS_HELP_STRING([--disable-hopes], [obsolete flag, do not use]),
    [AC_MSG_ERROR([

    The flag you used (--enable/disable-hopes) is obsolete and should not
    be used.  The VGOS tools are enabled by default in this distribution.
    If for some reason you do not want them, use:

      --disable-chops

    ], [2]) ], [])
###
AC_ARG_ENABLE(checks,
    AS_HELP_STRING([--disable-checks], [disables some sanity checking]),
    [], [enable_checks=yes])
AC_GBC_NOTICE([enable_checks is --$enable_checks--])
### option disabled
AC_ARG_ENABLE(pathx,
    AS_HELP_STRING([--enable-pathx], [try normal autoconf x11 path checks]))
AC_GBC_NOTICE([enable_pathx is --$enable_pathx--])
AC_ARG_ENABLE(xtrap,
    AS_HELP_STRING([--enable-xtrap], [try extra autoconf x11 path checks]))
AC_GBC_NOTICE([enable_xtrap is --$enable_xtrap--])

#
# An ISO time for installation tracking.
#
BUILD_DATE=`date -u +"%Y-%m-%dT%H:%M:%S"`
AC_SUBST(BUILD_DATE)

#
# Control the core HOPS post-processing tools
#
if [test x$enable_hops = xyes] ; then
    have_hops=true
else
    have_hops=false
fi
AC_GBC_NOTICE([%%% have_hops is $have_hops (required)])
AM_CONDITIONAL(HAVE_HOPS, $have_hops)

#
# Control the new CHOPS tools for VGOS
#
AC_ARG_VAR(USE_CHOPS, [variable to indicate if we have chops])
if [test x$enable_chops = xyes] ; then
    CHOPDIR='chops'
    have_chops=true
    USE_CHOPS=true
    AC_GBC_NOTICE([%%% have_chops is $have_chops (needed for VGOS)])
else
    CHOPDIR=''
    have_chops=false
    USE_CHOPS=false
    # no comment
fi
AC_SUBST(CHOPDIR)
AM_CONDITIONAL(HAVE_CHOPS, $have_chops)

# for help
AC_PROG_AWK
AC_PATH_PROG(HAVE_CSH, [csh], [have_csh=true] [have_csh=false])
AC_GBC_NOTICE([%%% have awk at [$AWK] and csh was found on [$PATH]])

have_devel=false
### option disabled
AC_SUBST(DEVLDIR)
AC_SUBST(PROGDIR)
AC_SUBST(IFFYDIR)
AC_SUBST(M4PYDIR)
AM_CONDITIONAL(HAVE_DEVEL, $have_devel)

##############################################################################
##############################################################################

# autoscan suggested list of checks follows

# Checks for programs.
AC_PATH_PROG([AUTORECONF], [autoreconf], [/bin/false])
AC_PATH_PROG([ACLOCAL], [aclocal], [/bin/false])
AC_PROG_CC
AC_PROG_F77
AC_PROG_LN_S
AC_PROG_RANLIB
AC_F77_LIBRARY_LDFLAGS
# this must follow AC_PROG_CC and AC_PROG_F77 for some reason
if [test x$enable_mark5 = xyes] ; then
    AC_PROG_CXX
else
    # define the conditionals AC_PROG_CXX normally does
    am__fastdepCXX_TRUE='#'
    am__fastdepCXX_FALSE='#'
fi
# HOPS prints and displays with ghostscript (gs)
# which might be at /usr/bin/gs, but which could be elsewhere
AC_PATH_PROG(GS_EXEC, [gs], [/bin/false])
AC_SUBST(GS_EXEC)
# copypage changed behavior between gs versions 9.05 and 9.10
# 8.7x (sci linux) copypage ok
# 9.05 (fedora 16) copypage ok
# 9.10 (MacPorts) copypage not ok
# 9.14 (fedora 19) copypage not ok
# 9.16 (fedora 22) copypage not ok
# and .outputpage stopped being the solution after 9.21?
# 9.22 (fedora 27) .outputpage no longer the solution
AC_ARG_VAR(GS_COPYPAGE_OK, [Set to 1 when gs version is prior to 9.10])
AC_ARG_VAR(GS_VERSION, [Set to the gs version number])
if [test "x$GS_EXEC" = "x/bin/false" ] ; then
    AC_GBC_NOTICE([%%% have_gs is false (required)])
    AC_GBC_NOTICE([PATH does not include ghostscript (gs)])
    GS_COPYPAGE_OK=1
    GS_VERSION=0.0
    AC_GBC_NOTICE([no gs so no gs version])
else
    AC_GBC_NOTICE([%%% have_gs is true (required)])
    AC_GBC_NOTICE([GS_EXEC is $GS_EXEC])
    gsvers=`$GS_EXEC --version`
    if [test -z "$GS_COPYPAGE_OK" ] ; then
        `echo $gsvers | awk '{if ($1 < 9.10) exit 1; else exit 0;}'`
        GS_COPYPAGE_OK=$?
#   modern autoconf macro:
#   AS_VERSION_COMPARE([$gsvers], [9.10],
#       [GS_COPYPAGE_OK=1], [GS_COPYPAGE_OK=0], [GS_COPYPAGE_OK=0])
        GS_VERSION=`echo $gsvers | awk '{printf "%.0f", $1 * 1000}'`
    fi
    AC_GBC_NOTICE([$GS_EXEC version is $gsvers, cf 9.10])
fi
AC_GBC_NOTICE([GS_COPYPAGE_OK is $GS_COPYPAGE_OK])
AC_GBC_NOTICE([GS_VERSION is $GS_VERSION])
AC_SUBST(GS_COPYPAGE_OK)
AC_SUBST(GS_VERSION)
# in theory we should eliminate GS_COPYPAGE_OK in favor of GS_VERSION

# and PS is so 20th century...
AC_PATH_PROGS(PS2PDF, ps2pdf, /bin/false)
AC_SUBST(PS2PDF)

# PERL/Python might be in nonstandard locations
AC_PATH_PROGS(PERL, perl, /bin/false)
#AC_PATH_PROGS(PYTHON, python, /bin/false)
AM_PATH_PYTHON([2.4],,[:])

# all the python tests should end up in an m4 test...
if [test "$PYTHON" != : -a "$pythondir" != ""] ; then
    # set defaults
    have_python=true
    extra_pass=0
    #check for package 'future'
    pyfuture_test=`$PYTHON -c "import future" 2>/dev/null; echo $?`
    if [test "$pyfuture_test" = 0 ]; then
        extra_pass="${extra_pass}1"
        AC_GBC_NOTICE([@@@ found python future requirement:  $extra_pass])
    else
        have_python=false
        AC_GBC_NOTICE([%%% Python has no future here:  $extra_pass])
    fi
    #check for package 'numpy'
    pynumpy_test=`$PYTHON -c "import numpy" 2>/dev/null; echo $?`
    if [test "$pynumpy_test" = 0 ]; then
        extra_pass=${extra_pass}2
        AC_GBC_NOTICE([@@@ found python numpy requirement:  $extra_pass])
    fi
    #check for package 'scipy'
    pyscipy_test=`$PYTHON -c "import scipy" 2>/dev/null; echo $?`
    if [test "$pyscipy_test" = 0 ]; then
        extra_pass=${extra_pass}3
        AC_GBC_NOTICE([@@@ found python scipy requirement:  $extra_pass])
    fi
    #check for package 'matplotlib'
    pymatplotlib_test=`$PYTHON -c "import matplotlib" 2>/dev/null; echo $?`
    if [test "$pymatplotlib_test" = 0 ]; then
        extra_pass=${extra_pass}4
        AC_GBC_NOTICE([@@@ found python matplotlib requirement:  $extra_pass])
    fi
    AC_GBC_NOTICE([@@@ extra_pass is $extra_pass])
    if [test "${extra_pass}" = '01234' ] ; then
        have_python_extra=true
        AC_GBC_NOTICE([%%% Python has all extras:  $extra_pass])
    else
        have_python_extra=false
        AC_MSG_WARN([
            You are missing one or more python packages that are
            needed for the VGOS Python scripts.  Install these
            and try again: future, numpy, scipy, and matplotlib.])
    fi
fi
AM_CONDITIONAL([HAVE_PYTHON], [$have_python])
AM_CONDITIONAL([HAVE_PYTHON_EXTRA], [$have_python_extra])

# .so on linux, .dylib on darwin
# libtool machinery on macos appears to define LT_SHARED_EXT as .dylib
# safest to do it ourselves
AC_GBC_NOTICE([version_type: $version_type])
if [test x$version_type = xdarwin ] ; then
    HOPSPYSHLEXT='.dylib'
else
    HOPSPYSHLEXT='.so'
fi
AC_SUBST(HOPSPYSHLEXT)


have_vex2xml=false
have_javas=false
if [test x$enable_vex2xml = xyes] ; then
    have_vex2xml=true
    VXMLDIR=vex2xml
    # Java Compiler for VEX2XML
    AC_PATH_PROGS(JAVA,  java,  /bin/false)
    AC_PATH_PROGS(JAVAC, javac, /bin/false)
    if [test "$JAVA" = '/bin/false'] ; then
        have_java=false
    else
        have_java=true
    fi
    if [test "$JAVAC" = '/bin/false'] ; then
        have_javac=false
    else
        have_javac=true
    fi
    AC_SUBST(JAVA)
    AC_SUBST(JAVAC)
else
    have_java=false
    have_javac=false
fi
if [test x$have_java = xtrue -a x$have_javac = xtrue] ; then
    have_javas=true
fi
AC_SUBST(VXMLDIR)
AM_CONDITIONAL([HAVE_JAVAS], [$have_javas])
AM_CONDITIONAL(HAVE_VEX2XML, [$have_vex2xml])
AC_GBC_NOTICE([%%% have_vex2xml is $have_vex2xml (optional)])

# only useful for vex2xml
if [test x$enable_antlr4 = xyes -a x$enable_devel = xyes ] ; then
    AC_MSG_WARN([note that the antlr4 grammer is broken....])
    ANTLR4SRC=src4
    ANTLR4LIB=lib4
    ANTLR4VER=4.8
    APACHECLI=1.4
else
#   AC_MSG_ERROR([you can only ask for antlr4 with --enable-devel], 1)
    if [test x$enable_antlr4 = xyes ] ; then
        AC_MSG_ERROR([you can only ask for antlr4 with --enable-devel])
    fi
    ANTLR4SRC=src
    ANTLR4LIB=lib
    ANTLR4VER=3.5.2
    APACHECLI=1.2
fi
AC_SUBST(ANTLR4SRC)
AC_SUBST(ANTLR4LIB)
AC_SUBST(ANTLR4VER)
AC_SUBST(APACHECLI)
antlermsg="$ANTLR4SRC/$ANTLR4LIB/$ANTLR4VER/$APACHECLI"
AC_GBC_NOTICE([%%% antlr src/lib/ver/cli: "$antlermsg" (developmental)])

# Checks for libraries.
AC_CHECK_LIB(m, floorl)

### Check for >64-bit long double
AC_TYPE_LONG_DOUBLE_WIDER

# Checks for header files.
if [$ac_hops] ; then
AC_FUNC_ALLOCA
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h fcntl.h float.h inttypes.h limits.h])
AC_CHECK_HEADERS([locale.h netdb.h netinet/in.h stddef.h stdint.h])
AC_CHECK_HEADERS([stdlib.h string.h strings.h sys/socket.h unistd.h])
AC_CHECK_HEADERS([sys/time.h sys/timeb.h])
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN(
    AC_DEFINE(WORDS_BIGENDIAN, 1, [this is a BigEndian Host]),
    AC_DEFINE(WORDS_BIGENDIAN, 0, [this is a LittleEndian Host])
)

# additional items for hops_config
# available if svn is the source and svn is installed and of proper version
svnexec=`type -p svn`
if [test -d $srcdir/.svn -a -n "$svnexec" -a -x "$svnexec"] ; then
    hops_svn_data=`"$svnexec" info "$srcdir" 2>&-` || hops_svn_data=''
else
    hops_svn_data=''
fi
if [test -n "$hops_svn_data"] ; then
    # provided by the svn repository: hops OR difx
    hops_svn_rev=`echo "$hops_svn_data" | grep Revision | cut -d' ' -f2`
    AC_GBC_NOTICE([hops SVN rev $hops_svn_rev via svn])
elif [test -f $srcdir/svnrev.txt] ; then
    # provided by disthook for tarballs
    hops_svn_rev=`cat $srcdir/svnrev.txt`
    AC_GBC_NOTICE([hops SVN rev $hops_svn_rev via svnrev.txt])
elif [test -f $srcdir/config.svnrev.txt] ; then
    # provided by nightly for client builds
    hops_svn_rev=`cat $srcdir/config.svnrev.txt`
    AC_GBC_NOTICE([hops SVN rev $hops_svn_rev via config.svnrev.txt])
else
    hops_svn_rev=0
    AC_GBC_NOTICE([hops SVN rev unknown, set to zero])
fi
if [test -z "$hops_svn_rev"] ; then
    hops_svn_rev=1
fi
AC_GBC_NOTICE([hops_svn_rev is $hops_svn_rev])
AH_TOP(
[/*
 * This file is at present included in relatively few HOPS sources, but
 * serves to identify and set macros that may be used in the future.
 */]
)
AC_DEFINE_UNQUOTED(HOPS_SVN_REV, [$hops_svn_rev], [Haystack SVN rev number])
AC_DEFINE_UNQUOTED(HOPS_REVISION, [$VERSION], [HOPS package rev number])
AH_BOTTOM(
[/* Worry about the byte order in data files */
#if WORDS_BIGENDIAN
/* Data are fine as they are */
#else /* WORDS_BIGENDIAN */
# define BYTFLP
#endif /* WORDS_BIGENDIAN */])
if [$ac_hops] ; then
AC_C_CONST
AC_C_INLINE
AC_C_VOLATILE
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_STRUCT_TM
fi

# Checks for library functions.
if [$ac_hops] ; then
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_MKTIME
AC_FUNC_MMAP
AC_FUNC_REALLOC
AC_FUNC_STAT
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([floor getcwd isascii memset mkdir modf pow rint sqrt])
AC_CHECK_FUNCS([strcasecmp strchr strdup strerror strncasecmp strrchr])
AC_CHECK_FUNC([hsearch_r], [have_hsearch=true], [have_hsearch=false])
AC_CHECK_FUNCS([hsearch_r strspn strstr])
AC_CHECK_FUNCS([clock_gettime gethostname gettimeofday munmap putenv])
AC_CHECK_FUNCS([regcomp strtol strtoul])
fi
AM_CONDITIONAL(HAVE_HSEARCH, $have_hsearch)

have_gsl=false
### option disabled
AM_CONDITIONAL(HAVE_GSL, $have_gsl)

#
# FFTW3 is now required
#
PKG_CHECK_MODULES(FFTW3, fftw3, [hasfftw=true], [hasfftw=false])
if [$hasfftw] ; then
    AC_DEFINE([HAVE_FFTW], 1, [Define to 1 if FFTW is available])
    AC_GBC_NOTICE([FFTW3 was found via check modules])
else
    AC_DEFINE([HAVE_FFTW], 0, [Define to 1 if FFTW is available])
    AC_GBC_NOTICE([FFTW3 not found via check modules])
fi
AC_GBC_NOTICE([%%% hasfftw is $hasfftw (required)])
AC_SUBST(FFTW3_CFLAGS)
AC_SUBST(FFTW3_LIBS)
AM_CONDITIONAL(HAVE_FFTW, $hasfftw)

AC_ARG_VAR(X_INSANE, [Anything you need to append to the X11 link chain])

# Check for X11 -- this isn't robust against all cases, but
# there are enough environment variables to cover many bases.
have_x11=false
# --enable-xtrap
if [test x${enable_pathx-no}${enable_xtrap-no} = xnoyes ] ; then
    AC_GBC_NOTICE([using ac_path_xtra method])
    AC_PATH_XTRA
    AC_SUBST(X_CFLAGS)
    AC_SUBST(X_PRE_LIBS)
    AC_SUBST(X_LIBS)
    AC_SUBST(X_IDIOT,'-lX11')
    AC_SUBST(X_EXTRA_LIBS)
    have_x11=true
    AC_GBC_NOTICE([X11 set via enable-xtrap])
fi
# --enable-pathx
if [test x${enable_pathx-no}${enable_xtrap-no} = xyesno ] ; then
    AC_GBC_NOTICE([using ac_path_x method])
    AC_PATH_X
    AC_SUBST(X_CFLAGS,'-I$x_includes')
    AC_SUBST(X_PRE_LIBS)
    AC_SUBST(X_LIBS,'-L$x_libraries')
    AC_SUBST(X_IDIOT,'-lX11')
    AC_SUBST(X_EXTRA_LIBS)
    have_x11=true
    AC_GBC_NOTICE([X11 set via enable-pathx])
fi
# --enable-pathx and --enable-xtrap or neither
if [test x$have_x11 = xfalse ] ; then
    AC_GBC_NOTICE([using default X11 method])
    AC_CHECK_LIB(X11, XOpenDisplay, have_x11=true, have_x11=false)
    AC_SUBST(X_CFLAGS)
    AC_SUBST(X_PRE_LIBS)
    AC_SUBST(X_LIBS,'-lX11')
    AC_SUBST(X_IDIOT)
    AC_SUBST(X_EXTRA_LIBS)
    AC_GBC_NOTICE([X11 set via check lib])
fi
AC_GBC_NOTICE([%%% have_x11 is $have_x11 (required)])
AM_CONDITIONAL(HAVE_X11, [test x$have_x11 = xtrue])
AC_SUBST(X11_LIB,
    '$(X_PRE_LIBS) $(X_LIBS) $(X_IDIOT) $(X_EXTRA_LIBS) $(X_INSANE)')

#
# Check for PGPLOT is complicated, and at present, not optional.
# pgxwin_server is needed for X11 installations and AC_PATH_PROG()
# is a convenient way to search among multiple directories.
# It is hard to get the search order right...in all cases.
#
# Further complicated by the DiFX use of the PGPLOTDIR variable.
# If that is defined and PGPLOT_DIR isn't, we'll take that as a hint.
# otherwise we'll fall back on /usr/local/pgplot as Tim intended.
#
# Further complicated by .so rather than .a
#
# The RPMfusion package provides
#   in /usr/lib{,64}:     libpgplot.so libcpgplot.so
#   in /usr/libexec:      pgxwin_server
#   in /usr/share/pgplot: grfont.dat  rgb.txt
# which violates the single-dir paradigm, but still perhaps works.
# And tcl-pgplot supplies (and other infrastructure)
#   in /usr/lib{,64}:     libtkpgplot.so
#
if [test -z "$PGPLOT_DIR" -a -n "$PGPLOTDIR" ] ; then
    pgplotdefault="$PGPLOTDIR"
else
    pgplotdefault='/usr/local/pgplot'
fi
# whether to use PGPLOT_RGB and PGPLOT_FONT
use_grfrgb=false
#
AC_ARG_VAR(PGPLOT_DIR, [PGPLOT installation directory])
AC_ARG_VAR(PGPLOT_FONT, [PGPLOT path to grfont.dat])
AC_ARG_VAR(PGPLOT_RGB, [PGPLOT path to rgb.txt])
if [test x${enable_pgplot-yes} = xyes ] ; then
    AC_GBC_NOTICE([searching for pgxwin_server])
    AC_PATH_PROG(PGXWIN_SERVER, pgxwin_server, false,
	[${PGPLOT_DIR-"$pgplotdefault"} /usr/lib/pgplot5 dnl
	 /usr/lib/pgplot64 /usr/local/pgplot-png /usr/libexec/pgplot dnl
	 /usr/bin /opt/local/bin /sw/lib/pgplot])
    pgpdir=`AS_DIRNAME(["${PGXWIN_SERVER-/no-such-dir}"])`
    if [test "$pgpdir" = '/usr/bin'] ; then
	# for Debian packages
	pgpdir='/usr/lib'
        pgplib=$pgpdir/libpgplot.a
        if [test -f $pgpdir/libpgplot.so ] ; then
            pgplib=$pgpdir/libpgplot.so
        fi
	cpglib=$pgpdir/libcpgplot.a
        if [test -f $pgpdir/libcpgplot.so ] ; then
            cpglib=$pgpdir/libcpgplot.so
        fi
	grfont=${PGPLOT_DIR-'/usr/lib/pgplot5'}/grfont.dat
	rgbtxt=${PGPLOT_DIR-'/usr/lib/pgplot5'}/rgb.txt
	AC_GBC_NOTICE([pgpdir was /usr/bin, revised to $pgpdir])
    elif [test "$pgpdir" = '/opt/local/bin'] ; then
	# for MacPorts
	pgpdir='/opt/local/lib'
        pgplib=$pgpdir/libpgplot.a
        if [test -f $pgpdir/libpgplot.so ] ; then
            pgplib=$pgpdir/libpgplot.so
        fi
	cpglib=$pgpdir/libcpgplot.a
        if [test -f $pgpdir/libcpgplot.so ] ; then
            cpglib=$pgpdir/libcpgplot.so
        fi
	grfont=/opt/local/share/pgplot/grfont.dat
	rgbtxt=/opt/local/share/pgplot/rgb.txt
	AC_GBC_NOTICE([pgpdir was /opt/local, revised to $pgpdir])
    elif [test "$pgpdir" = /usr/libexec/pgplot ] ; then
        # assume an RPM Fusion or similar installation
        pgpdir='/usr/libexec/pgplot'
        pgplib=`ls /usr/lib*/libpgplot.so`
        cpglib=`ls /usr/lib*/libcpgplot.so`
        grfont=`ls /usr/share/pgplot/grfont.dat`
        rgbtxt=`ls /usr/share/pgplot/rgb.txt`
	AC_GBC_NOTICE([pgpdir is $pgpdir, assuming RPM Fusion or similar])
    else
	# well, no pgxwin_server, but PGPLOT may yet be functional
	if [test "$pgpdir" = '/no-such-dir'] ; then
	    pgpdir=${PGPLOT_DIR="$pgplotdefault"}
	fi
        pgplib=$pgpdir/libpgplot.a
        if [test -f $pgpdir/libpgplot.so ] ; then
            pgplib=$pgpdir/libpgplot.so
        fi
	cpglib=$pgpdir/libcpgplot.a
        if [test -f $pgpdir/libcpgplot.so ] ; then
            cpglib=$pgpdir/libcpgplot.so
        fi
	grfont=$pgpdir/grfont.dat
	rgbtxt=$pgpdir/rgb.txt
        AC_GBC_NOTICE([last resort: pgpdir is $pgpdir])
    fi
    if [test -d $pgpdir -a -f $pgplib -a -f $cpglib -a $grfont -a $rgbtxt];then
	AC_GBC_NOTICE([PGPLOT_DIR is $pgpdir])
	have_pgplot=true
        # FLIBS should be the fortran libes used to compile pgplot
        if [test "$pgpdir" = /usr/libexec/pgplot ] ; then
            PGPLOT_DIR=''
            PGP_LIB=['-lcpgplot -lpgplot $(FLIBS)']
            use_grfrgb=true
        else
            PGPLOT_DIR=$pgpdir
            # PGP_LIB=['$(FLIBS) -L$(PGPLOT_DIR) -lcpgplot -lpgplot']
            PGP_LIB=['-L$(PGPLOT_DIR) -lcpgplot -lpgplot $(FLIBS)']
        fi
    else
	AC_GBC_NOTICE([PGPLOT was too hard to find.])
	AC_GBC_NOTICE([Try setting PGPLOT_DIR to point to a directory])
	AC_GBC_NOTICE([that contains symbolic links to the installed])
	AC_GBC_NOTICE([files libpgplot.a or .so, libcpgplot.a or .so,])
        AC_GBC_NOTICE([grfont.dat, rgb.txt and pgxwin_server.  If any])
        AC_GBC_NOTICE([of these are not present in PGPLOT_DIR, there])
        AC_GBC_NOTICE([will be problems.  See README.pgplot.txt.])
	have_pgplot=false
	PGPLOT_DIR=''
	PGP_LIB=''
    fi
    # does this PGPLOT compilation require PNG(12)?
    # actually this test only works for a static archive and doesn't
    # help for a dynamic archive
    need_png=optional
    is_arch=`expr $pgplib : '.*\.a'`
    if [test -f $pgplib -a $is_arch -gt 10 ] ; then
	pndriv=`ar t $pgplib pndriv.o`
	if [test x$pndriv = xpndriv.o] ; then
	    need_png=required
	fi
        AC_GBC_NOTICE([is_arch is $is_arch pgplib is $pgplib])
        AC_GBC_NOTICE([pndriv is $pndriv need_png is $need_png])
    fi
fi
AC_GBC_NOTICE([%%% have_pgplot is $have_pgplot ($need_png)])
AM_CONDITIONAL(HAVE_PGPLOT, [test x$have_pgplot = xtrue])
AC_SUBST(PGP_LIB)
AC_SUBST(PGPLOT_DIR)
if [$use_grfrgb] ; then
    AC_GBC_NOTICE([%%% assigning PGPLOT_FONT and PGPLOT_RGB:])
    AC_GBC_NOTICE([%%% PGPLOT_FONT file expected at $grfont])
    AC_GBC_NOTICE([%%% PGPLOT_RGB file expected at $rgbtxt])
else
    AC_GBC_NOTICE([%%% not assigning PGPLOT_FONT and PGPLOT_RGB:])
    AC_GBC_NOTICE([%%% PGPLOT_FONT file expected at $grfont])
    AC_GBC_NOTICE([%%% PGPLOT_RGB file expected at $rgbtxt])
    grfont=''
    rgbtxt=''
fi
PGPLOT_FONT=$grfont
if [ -n "$PGPLOT_FONT" ] ; then
    AC_GBC_NOTICE([PGP FONT is `ls -l $PGPLOT_FONT 2>&1`])
fi
PGPLOT_RGB=$rgbtxt
if [ -n "$PGPLOT_RGB" ] ; then
    AC_GBC_NOTICE([PGP RGB  is `ls -l $PGPLOT_RGB 2>&1`])
fi
AC_SUBST(PGPLOT_FONT)
AC_SUBST(PGPLOT_RGB)

# Check for PNG (primitive) which MAY be required by pgplot
# One could provide the linkage stubs if PNG is required, but missing.
# If built within the DiFX environment, it might be available.
AC_ARG_VAR(PNG12_LIB, [Path to libpng12 shared object])
# this test could be skipped if need_png is optional
AC_CHECK_LIB(png12, png_access_version_number, have_png=true, have_png=false)
if [$have_png] ; then
    AC_GBC_NOTICE([Found png12 normally])
    PNG_LIB='-lpng12'
elif [test $need_png = required]; then
    AC_GBC_NOTICE([Looking harder for png12 need_png is $need_png])
    if [test -z "$PNG12_LIB" -a -n "$DIFX_ARCH_ROOT"]; then
	PNG12_LIB=${DIFX_ARCH_ROOT}/lib/libpng12.so.0
        AC_GBC_NOTICE([set png12 lib via DiFX/lib])
    elif [test -z "$PNG12_LIB" ]; then
	PNG12_LIB=/usr/lib/libpng12.so.0
        AC_GBC_NOTICE([set png12 lib via usr/lib])
    fi
    if [test -f "$PNG12_LIB" ] ; then
	have_png=true
	PNG_LIB=$PNG12_LIB
        AC_GBC_NOTICE([set png lib via png12])
    else
	have_png=false
	PNG_LIB=''
    fi
else
    AC_GBC_NOTICE([Skipping png12 since need_png is $need_png])
fi
AC_GBC_NOTICE([%%% have_png is $have_png ($need_png)])
AM_CONDITIONAL(HAVE_PNG, [test x$have_png = xtrue -o x$need_png = xoptional])
AC_SUBST(PNG_LIB)

#
# HOPS-specific Makefile substitutions
# The _LIB variables are to be listed on _LDADD lines
# The _DEP variables are to be given to _DEPENDENCIES
# The AC_GBC_LIBDEP([name],[dir],[lib]) macro computes 2 substitutions for name
#
HOPS_INC=['-I$(top_srcdir)/include']
AC_SUBST(HOPS_INC)
AC_GBC_LIBDEP(MK4IO,sub/mk4py,mk4io)
AC_GBC_LIBDEP(AFIO,sub/afio,afio)
AC_GBC_LIBDEP(DFIO,sub/dfio,dfio)
AC_GBC_LIBDEP(UTIL,sub/util,mk4util)
AC_GBC_LIBDEP(VEX,sub/vex,vex)

# There once was a library (postproc/sub/x_fplot)
# but apparently it has not been used in a long time.
AC_SUBST(X_FPLOT_LIB,[''])

#
# Apple always has to be different.
# This forces more things to be loaded.
#
if [ test x$build_vendor = xapple ] ; then
    LDFLAGS="-all_load $LDFLAGS"
fi

have_ompi=false
### option disabled
AC_SUBST(OMPIDIR)
AM_CONDITIONAL(HAVE_OMPI, [test $have_ompi = true])

have_docs=false
have_doxy=false
DOCSDIR=''
### option disabled
AC_SUBST(DOCSDIR)
AM_CONDITIONAL(HAVE_DOCS, [test $have_docs = true])
AM_CONDITIONAL(HAVE_DOXY, [test $have_doxy = true])

have_difx=false
### option disabled
AC_SUBST(DIFXDIR)
AM_CONDITIONAL(HAVE_DIFX, [test $have_difx = true])

have_mark5=false
### option disabled
AC_SUBST(MARK5DIR)
AM_CONDITIONAL(HAVE_MARK5, $have_mark5)

#
# Do you want to live on the edge?
# Use this to bypass explicit calls to AC_MSG_ERROR([cannot...], N)
#
if [test x$enable_checks = xyes] ; then
  if [test x$have_hops = xtrue] ; then
    if [test x$have_gs = xfalse] ; then
	AC_MSG_ERROR([halted because of GHOSTSCRIPT issues], 1)
    elif [test x$have_x11 = xfalse] ; then
	AC_MSG_ERROR([halted because of X11 issues], 2)
    elif [test x$have_pgplot = xfalse] ; then
	AC_MSG_ERROR([halted because of PGPLOT issues], 3)
    elif [test x$have_png = xfalse -a x$need_png = xrequired] ; then
	AC_MSG_ERROR([halted because of PNG issues], 4)
    elif [test x$hasfftw = xfalse] ; then
        AC_MSG_ERROR([halted because FFTW is missing], 5)
    else
	AC_GBC_BANNER([Configure passes sanity checks.])
    fi
  else
    AC_GBC_BANNER([Sanity checks disabled.])
  fi
fi


##############################################################################
##############################################################################


#
# Always make these files....
#
AC_CONFIG_FILES([
    Makefile
	help/Makefile
    README.txt
])


#
# The core HOPS postprocessing tools
#

if [ "$have_hops" = true ] ; then
    AC_CONFIG_FILES([
	include/Makefile
	scripts/Makefile
	correlator/Makefile
	correlator/CorAsc2/Makefile
	data/Makefile
	data/ff_testdata/Makefile
	data/ae_testdata/Makefile
	postproc/Makefile
	postproc/adump/Makefile
	postproc/aedit/Makefile
	postproc/alist/Makefile
	postproc/fourfit/Makefile
	postproc/fourmer/Makefile
	postproc/fplot/Makefile
	postproc/fringex/Makefile
	postproc/snratio/Makefile
	sub/Makefile
	sub/afio/Makefile
	sub/dfio/Makefile
	sub/dfio/copypage/Makefile
	sub/util/Makefile
	sub/vex/Makefile
    ])
    AC_CONFIG_FILES(
	[scripts/hops_data_links.pl],
	[chmod +x scripts/hops_data_links.pl]
    )
    AC_CONFIG_FILES(
	[scripts/vhelp.sh],
	[chmod +x scripts/vhelp.sh]
    )
    AC_CONFIG_FILES(
        [scripts/fourphase_original],
        [chmod +x scripts/fourphase_original]
    )
    AC_CONFIG_FILES(
        [scripts/est_manual_phases.py],
        [chmod +x scripts/est_manual_phases.py]
    )
    AC_CONFIG_FILES(
        [scripts/pcplot],
        [chmod +x scripts/pcplot]
    )
    AC_CONFIG_FILES(
        [scripts/tpcplot],
        [chmod +x scripts/tpcplot]
    )
    if [ $have_python = "true" ] ; then
        AC_CONFIG_FILES( [data/ff_testdata/hopstestsuite.py])
    fi
fi

# this was under development, but is now available by default
if [ "$have_vex2xml" = true ] ; then
    AC_CONFIG_FILES([vex2xml/Makefile])
fi

### option disabled

#
# And any HOP(Enhanced)S toys.
# aka CHOPS (chopped up HOPS).
#
if [ "$have_chops" = true ] ; then
    AC_CONFIG_FILES([
     chops/Makefile
     chops/source/Makefile
     chops/source/c_src/Makefile
     chops/source/c_src/mk4util/Makefile
     chops/source/c_src/dfio/Makefile
     chops/source/c_src/afio/Makefile
     chops/source/c_src/vex/Makefile
     chops/source/c_src/fourfit_libs/Makefile
     chops/source/c_src/fourfit_libs/ffcontrol/Makefile
     chops/source/c_src/fourfit_libs/ffcore/Makefile
     chops/source/c_src/fourfit_libs/ffio/Makefile
     chops/source/c_src/fourfit_libs/ffmath/Makefile
     chops/source/c_src/fourfit_libs/ffsearch/Makefile
     chops/source/c_src/fourfit_libs/ffplot/Makefile
     chops/source/c_src/applications/Makefile
     chops/source/c_src/applications/fourfit/Makefile
     chops/source/c_src/applications/alist/Makefile
     chops/source/c_src/applications/aedit/Makefile
    ])
    AC_CONFIG_FILES([chops/hops_checksum.sh], [chmod +x chops/hops_checksum.sh])
    AC_CONFIG_FILES([chops/run_checksum.sh], [chmod +x chops/run_checksum.sh])

    # These need to be configured, whether used or not.
    AC_CONFIG_FILES( chops/source/python_src/Makefile )
    # This one is a challenge.  Here we make includes optional
    AC_CONFIG_FILES([chops/source/python_src/mk4_module/Makefile],
        [mv chops/source/python_src/mk4_module/Makefile chops/source/python_src/mk4_module/Makefile- ; sed 's/^include src/-include src/' chops/source/python_src/mk4_module/Makefile- > chops/source/python_src/mk4_module/Makefile ; rm chops/source/python_src/mk4_module/Makefile-])
    AC_CONFIG_FILES( chops/source/python_src/hopstest_module/Makefile )
    AC_CONFIG_FILES( chops/source/python_src/ffcontrol_module/Makefile )
    AC_CONFIG_FILES( chops/source/python_src/vpal_module/Makefile )
    AC_CONFIG_FILES([chops/source/python_src/vex_module/Makefile],
        [mv chops/source/python_src/vex_module/Makefile chops/source/python_src/vex_module/Makefile- ; sed 's/^include src/-include src/' chops/source/python_src/vex_module/Makefile- > chops/source/python_src/vex_module/Makefile ; rm chops/source/python_src/vex_module/Makefile-])
    AC_CONFIG_FILES( chops/source/python_src/scripts/Makefile )
    AC_CONFIG_FILES( chops/source/python_src/tests/Makefile )
    AC_CONFIG_FILES( [chops/source/python_src/mk4_module/setup.py] )
    AC_CONFIG_FILES( [chops/source/python_src/hopstest_module/setup.py] )
    AC_CONFIG_FILES(
        [chops/source/python_src/hopstest_module/hopstestsuiteb.py] )
    AC_CONFIG_FILES( [chops/source/python_src/ffcontrol_module/setup.py] )
    AC_CONFIG_FILES( [chops/source/python_src/vpal_module/setup.py] )
    AC_CONFIG_FILES( [chops/source/python_src/vex_module/setup.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/pcc_generate.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/pcc_select.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/pcc_plot.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/vgoscf_generate.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/phase_resid.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/batch_fourfit.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/ffres2pcp.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/fourphase.py] )
    AC_CONFIG_FILES( [chops/source/python_src/scripts/summarize_report.py])
    AC_CONFIG_FILES( [chops/source/python_src/scripts/select_bandpols.py] )
    AC_CONFIG_FILES( [chops/source/python_src/tests/test_harness.sh],
        [chmod +x chops/source/python_src/tests/test_harness.sh] )
    AC_DEFINE_UNQUOTED([PYTHON_SITE_PREFIX], ["$pythondir"],
        [Python install dir])

fi


#
# Shell setup convenience
#
AC_CONFIG_FILES(hops.bash,
    [chmod +x hops.bash;
     echo "";
     echo "to set up your environment, now and in the future:";
     echo "  "source ./hops.bash;
     echo "";
    ])

AC_OUTPUT

#
# eof
#
