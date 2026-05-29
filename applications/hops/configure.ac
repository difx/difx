#
# Process this file with autoconf to produce a configure script.
# Run autoscan to get suggestions on more things to check.
#
# Note that HOPS doesn't presently use hops_config.h except
# to detect endianism in bytflp.h for some of the tools.
#
# The ###HEAD and ###TAIL constructs eliminate blocks of configure
# from the vendor branch supplied to DiFX via a configure_ac.
#
# Autoconf 2.59 was released 2003-12-16 15:52 and remains quite viable.
# AC_GBC* m4 macros are defined in m4/gbc.m4 and exist for readability
#
AC_PREREQ([2.59])
AC_INIT([hops],[3.26],[gbc@mit.edu])
AM_INIT_AUTOMAKE([foreign -Wno-extra-portability subdir-objects])

AC_CONFIG_SRCDIR([hops.bash.in])
AC_CONFIG_HEADERS([hops_config.h])
AC_CONFIG_MACRO_DIR([m4])
m4_ifset([LT_INIT], [LT_INIT], [AC_PROG_LIBTOOL])
m4_ifset([LT_WITH_LTDL], [LT_WITH_LTDL], [AC_LIB_LTDL])

AC_PROG_CC

# ignore this line which initializes config.summary
AC_GBC_INIT([A digest of configuration details],[config.summary])

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
#   --enable-FEATURE  -> enableval=$enable_FEATURE and enable_FEATURE=yes
#   --disable-FEATURE -> enableval=$enable_FEATURE and enable_FEATURE=no
#   --disable-FEATURE=anything              turns into enable_FEATURE=no
#
AC_ARG_ENABLE(hops,
    AS_HELP_STRING([--disable-hops], [disables the post-processing tools]),
    [], [enable_hops=yes])
AC_GBC_NOTICE([enable_hops is --$enable_hops--])

### option disabled

# this is required for VGOS processing in HOPS3
AC_ARG_ENABLE(chops,
    AS_HELP_STRING([--disable-chops], [disables the vgos-processing tools]),
    [], [enable_chops=yes])
AC_GBC_NOTICE([enable_chops is --$enable_chops--])

# this was developed for ALMA and is captured here in case anyone wants it
AC_ARG_ENABLE(vex2xml,
    AS_HELP_STRING([--enable-vex2xml], [enable VEX2XML tool]),
    [], [enable_vex2xml=yes])
AC_GBC_NOTICE([enable_vex2xml is --$enable_vex2xml--])
# this one is provided, but not yet implmented
AC_ARG_ENABLE(antlr4,
    AS_HELP_STRING([--enable-antlr4], [enable Antlr4 used by vex2xml]),
    [], [enable_antlr4=no])
AC_GBC_NOTICE([enable_antlr4 is --$enable_antlr4--])

### option disabled
#
# the following support broken X11 implementations
AC_ARG_ENABLE(pathx,
    AS_HELP_STRING([--enable-pathx], [try normal autoconf x11 path checks]))
AC_GBC_NOTICE([enable_pathx is --$enable_pathx--])
AC_ARG_ENABLE(xtrap,
    AS_HELP_STRING([--enable-xtrap], [try extra autoconf x11 path checks]))
AC_GBC_NOTICE([enable_xtrap is --$enable_xtrap--])

# newer installations of PGPLOT may not need png12...
AC_ARG_ENABLE(png12,
    AS_HELP_STRING([--disable-png12], [disables the png12 checking]),
    [], [enable_png12=yes])
AC_ARG_ENABLE(checks,
    AS_HELP_STRING([--disable-checks], [disables some sanity checking]),
    [], [enable_checks=yes])
AC_GBC_NOTICE([enable_checks is --$enable_checks--])

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

# awk is needed for vhelp processing
AC_PROG_AWK

# these are conveniences
AC_PROG_EGREP
AC_PATH_PROG(CSH, [csh], [have_csh=true] [have_csh=false])
AC_PATH_PROG(TR, [tr], [have_tr=true] [have_tr=false])
AC_GBC_NOTICE([have awk at [$AWK]])
if [$have_csh] ; then
    AC_GBC_NOTICE([have csh at [$CSH]])
fi
if [$have_tr] ; then
    AC_GBC_NOTICE([have tr at [$TR]])
    for p in `echo $PATH | tr \: ' '` ; do
        AC_GBC_NOTICE([PATH has: $p])
    done
fi
AC_GBC_NOTICE([EGREP is $EGREP])

### option disabled
AC_SUBST(DEVLDIR)
AC_SUBST(PROGDIR)
AC_SUBST(IFFYDIR)
#AC_SUBST(M4PYDIR)
AM_CONDITIONAL(HAVE_DEVEL, false)
AM_CONDITIONAL(PURGE_NR, true)

##############################################################################
##############################################################################

# autoscan suggested list of checks follows

# Checks for programs.
AC_PATH_PROG([AUTORECONF], [autoreconf], [/bin/false])
AC_PATH_PROG([ACLOCAL], [aclocal], [/bin/false])
AC_PROG_CC
# we have sparing use of fortran and gfortran is now preferred
AC_PROG_F77(gfortran g77 f77)
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
# these are used in cohfit
AC_PATH_PROGS(GNUPLOT, gnuplot, /bin/false)
AC_SUBST(GNUPLOT)
AC_PATH_PROGS(MONTAGE, montage, /bin/false)
AC_SUBST(MONTAGE)

# GSL is now required for cohfit, but the bspline infrastucture is
# different in 2.7 and 2.8; first check for basic gsl:
#
#  The following was the original setup for noise generation in misc code
#  which is a good start in any case as these calls have been with GSL
#  for a very long time.
AC_CHECK_HEADERS( [gsl/gsl_blas.h gsl/gsl_bspline.h gsl/gsl_errno.h gsl/gsl_math.h gsl/gsl_matrix.h gsl/gsl_min.h gsl/gsl_multifit.h gsl/gsl_multifit_nlinear.h gsl/gsl_multifit_nlin.h gsl/gsl_randist.h gsl/gsl_rng.h gsl/gsl_statistics.h gsl/gsl_vector.h])
AC_CHECK_LIB(gsl, gsl_ran_gaussian,
    [AC_DEFINE(HAVE_LIBGSL,1,[Define to 1 with -lgsl (noise testing)])
     GSL_LIBS='-lgsl -lgslcblas'
     have_gsl=true], [have_gsl=false], [-lgslcblas])
AC_CHECK_LIB(gsl, gsl_bspline_eval,
    [AC_DEFINE(HAVE_GSL_BSPLINE_EVAL,1,[Define to 1 if legacy splines supported])
     GSL_LIBS='-lgsl -lgslcblas'
     have_gsl_bspline_eval=true],
     [have_gsl_bspline_eval=false], [-lgslcblas])
AC_GBC_NOTICE([%%% have_gsl_bspline_eval is $have_gsl_bspline_eval])
AC_CHECK_LIB(gsl, gsl_bspline_calc,
    [AC_DEFINE(HAVE_GSL_BSPLINE_CALC,1,[Define to 1 if modern splines supported])
     GSL_LIBS='-lgsl -lgslcblas'
     have_gsl_bspline_calc=true],
     [have_gsl_bspline_calc=false], [-lgslcblas])
AC_GBC_NOTICE([%%% have_gsl_bspline_calc is $have_gsl_bspline_calc])
AC_SUBST(GSL_LIBS)

# we assume here that m4/gsl.m4 got loaded by aclocal
# the bspline code changed 2.7 to 2.8 (with 2.8 still supporting 2.7)
# gsl_bspline_eval() v gsl_bspline_calc() is a suitable test in
# AC_CHECK_LIB, but for the moment, we'll be stricter.
#
# We did the initial development on 2.8, then verified a solution at 2.7
# using the older basis splines introduce with 2.8.
# gsl/gsl_multifit_nlinear.h appears to have been introduced in 2.2
# the older gsl_multifit_nlin.h exists on 1.15.
#
# we have verified this on a 1.15 sustem
#
AC_PATH_PROG(BASH, bash, /bin/false)
if $have_gsl ; then
#   AX_PATH_GSL(2.7,  [gsl27atleast=true], gsl27atleast=false])
    AX_PATH_GSL(1.15, [gsl115atleast=true], gsl115atleast=false])
    AC_PATH_PROG(GSL_CONFIG, gsl-config, no)
    if [test test "$GSL_CONFIG" = "no"] ; then
        GSL_VERSION=0.0
    else
        GSL_VERSION=`$GSL_CONFIG --version`
    fi
    AS_VERSION_COMPARE($GSL_VERSION, 1.15, gs115=false, gs115=true, gs115=true)
    AS_VERSION_COMPARE($GSL_VERSION, 2.2,  gsl22=false, gsl22=true, gsl22=true)
    AS_VERSION_COMPARE($GSL_VERSION, 2.7,  gsl27=false, gsl27=true, gsl27=true)
    AS_VERSION_COMPARE($GSL_VERSION, 2.8,  gsl28=false, gsl28=true, gsl28=true)
fi
AM_CONDITIONAL(HAVE_GSL, $have_gsl)
AM_CONDITIONAL(HAVE_GSL115, $gs115)
AM_CONDITIONAL(HAVE_GSL_22, $gsl22)
AM_CONDITIONAL(HAVE_GSL_27, $gsl27)
AM_CONDITIONAL(HAVE_GSL_28, $gsl28)
AC_GBC_NOTICE(
    [%%% GSL conditionals (1.15 2.2 2.7 2.8): $gs115 $gsl22 $gsl27 $gsl28])

# PERL might be in nonstandard locations
AC_PATH_PROGS(PERL, perl, /bin/false)

# PYTHON version is no longer particularly relevant
# The last usable 2.x python was 2.7.
# All python should be 3.x compatible via future.
AM_PATH_PYTHON([2.4],,[:])
AC_DEFINE_UNQUOTED([PYTHON_SITE_PREFIX], ["$pythondir"], [Python install dir])

# all the python tests should end up in an m4 test...
have_python=false
have_python_extra=false
# future constructs will appear through 3.12 and be commented out after
PYTHON_FUTURE=''
PYTHON_OLDDVA='past.utils'
PYTHON_OLDDVB='past.utils'
if [test "$PYTHON" != : -a "$pythondir" != ""] ; then
    pyc='import sys ; print((sys.version_info.major))'
    farthing=`$PYTHON -c "$pyc" 2>/dev/null`
    AC_GBC_NOTICE([@@@ farthing is .$farthing.])
    # set defaults
    have_python=true
    extra_pass=0
    #check for package 'future'
    pyfuture_test=`$PYTHON -c "import future" 2>/dev/null; echo $?`
    if [test "$pyfuture_test" = 0 ]; then
        extra_pass="${extra_pass}1"
        AC_GBC_NOTICE([@@@ found python future requirement:  $extra_pass])
        PYTHON_OLDIVA='past.utils'
        PYTHON_OLDIVB='past.utils'
    else
      if [test x"$farthing" = x"3"] ; then
        extra_pass="${extra_pass}1"
        AC_GBC_NOTICE([@@@ no python future so 3.13 or later:  $extra_pass])
        PYTHON_FUTURE='#-3.13++#'   #an easily found marker
        PYTHON_OLDIVA='mk4'
        PYTHON_OLDIVB='mk4b.mk4b'
      else
        have_python=false
        AC_GBC_NOTICE([%%% Python has no future here:  $extra_pass])
      fi
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
    #check for package 'pip'
    pypip_test=`$PYTHON -c "import pip" 2>/dev/null; echo $?`
    $PYTHON -m pip 2>/dev/null 1>&2
    pypi2_test=$?
    if [test "$pypip_test$pypi2_test" = '00' ]; then
        extra_pass=${extra_pass}5
        AC_GBC_NOTICE([@@@ found python pip requirement:  $extra_pass])
    else
        if [test "$pypip_test" -eq 0]; then
            AC_GBC_NOTICE([@@@ have pip but is not executable: $pypi2_test])
        fi
    fi
    AC_GBC_NOTICE([@@@ extra_pass is $extra_pass])
    if [test "${extra_pass}" = '012345' ] ; then
        have_python_extra=true
        AC_GBC_NOTICE([%%% Python has all extras:  $extra_pass])
    else
        have_python_extra=false
        AC_MSG_WARN([
            You are missing one or more python packages that are
            needed for the VGOS Python scripts.  Install these
            and try again: pip, future, numpy, scipy, and matplotlib.])
    fi
    # we can probably just use PYTHON_VERSION for PYTHON_MAJOR_MINOR
    pyimports='import future;import sys'
    pycc='print(type(sys.version_info))'
    tuppence=`$PYTHON -c "$pyimports ; $pycc" 2>/dev/null`
    AC_GBC_NOTICE([@@@ Python version: .$PYTHON_VERSION. tuppence .$tuppence.])
    if [test x"$tuppence" = x"<type 'tuple'>"] ; then
        AC_GBC_NOTICE([@@@ Python version is: 2.6 or earlier])
        PYTHON_MAJOR_MINOR=$PYTHON_VERSION
        PYTHON_MAJOR=2
        AC_GBC_NOTICE([@@@ Python $PYTHON_MAJOR_MINOR and $PYTHON_MAJOR])
    else
      if [test x"$tuppence" = x"<class 'sys.version_info'>" ] ; then
        AC_GBC_NOTICE([@@@ Python version is: 3.0 through 3.12])
        pyprintcmd='print("%d.%d" % ' ; pysv='sys.version_info'
        pyprintcmd="$pyprintcmd($pysv.major,$pysv.minor))"
        PYTHON_MAJOR_MINOR=`$PYTHON -c "$pyimports ; $pyprintcmd"`
        PYTHON_MAJOR=`$PYTHON -c "$pyimports;print((sys.version_info.major))"`
        AC_GBC_NOTICE([@@@ Python $PYTHON_MAJOR_MINOR and $PYTHON_MAJOR])
      else
       if [ x"$tuppence" = x"<type 'sys.version_info'>" ] ; then
         AC_GBC_NOTICE([@@@ Python version is: 2.7 or later])
         pyprintcmd='print("%d.%d" % ' ; pysv='sys.version_info'
         pyprintcmd="$pyprintcmd($pysv.major,$pysv.minor))"
         PYTHON_MAJOR_MINOR=`$PYTHON -c "$pyimports ; $pyprintcmd"`
         PYTHON_MAJOR=`$PYTHON -c "$pyimports;print((sys.version_info.major))"`
         AC_GBC_NOTICE([@@@ Python $PYTHON_MAJOR_MINOR and $PYTHON_MAJOR])
       else
        if [test x"$farthing" = x"3"] ; then
         AC_GBC_NOTICE([@@@ Python version is: 3.13 or later])
         PYTHON_MAJOR=$farthing
         pyprintcmd='print("%d.%d"%(sys.version_info.major'
         pyprintcmd="$pyprintcmd"',sys.version_info.minor))'
         PYTHON_MAJOR_MINOR=`$PYTHON -c "import sys ; $pyprintcmd"`
         AC_GBC_NOTICE([@@@ Python $PYTHON_MAJOR_MINOR and $PYTHON_MAJOR])
        else
         AC_GBC_NOTICE([@@@ Python version is: .$PYTHON_VERSON.])
         PYTHON_MAJOR=
         PYTHON_MAJOR_MINOR=$PYTHON_VERSION
         AC_GBC_NOTICE([@@@ Python $PYTHON_MAJOR_MINOR and $PYTHON_MAJOR])
        fi
       fi
      fi
    fi
    # but one does not trust python
    if [test x$PYTHON_VERSION = x$PYTHON_MAJOR_MINOR] ; then
        AC_GBC_NOTICE([%%% Python version is: $PYTHON_MAJOR_MINOR, sane])
    else
        AC_GBC_NOTICE([%%% Python version is: $PYTHON_MAJOR_MINOR, insane])
    fi
    AC_GBC_NOTICE([%%% Python version: $PYTHON_MAJOR])
fi
# cover for busted python installations
if [test x"$PYTHON_MAJOR" = x ] ; then
    have_python=false
    AC_GBC_NOTICE([
        Your system appears to have a broken python installation
        This is known to happen in 2.7 systems without future pkg.
    ])
fi
AM_CONDITIONAL([HAVE_PYTHON], [$have_python])
AM_CONDITIONAL([HAVE_PYTHON_EXTRA], [$have_python_extra])
AC_SUBST(PYTHON_MAJOR_MINOR)
AC_SUBST(PYTHON_MAJOR)
AC_SUBST(PYTHON_FUTURE)
AC_SUBST(PYTHON_OLDIVA)
AC_SUBST(PYTHON_OLDIVB)

# with 3.11 setup install is deprecated, and gone in 3.12
# this next block of code puts the install directory in PYTHONPATH
# and that is sufficient for setup install to function
if [$have_tr] ; then
    pysite="${prefix}/lib"
    pysite="$pysite/python${PYTHON_MAJOR_MINOR}/site-packages"
    have_pysite=false
    for p in `echo $PYTHONPATH | tr \: ' '` ; do
        AC_GBC_NOTICE([PYTHONPATH has: $p])
        if [test $p = $pysite -o $p = $pysite/ ] ; then
            have_pysite=true
        fi
    done
    if [test $have_pysite = false ] ; then
        if [test -n "$PYTHONPATH" ] ; then
            export PYTHONPATH=${pysite}:$PYTHONPATH
        else
            export PYTHONPATH=${pysite}
        fi
        export PYTHONPATH=${pysite}/hops/:$PYTHONPATH
        AC_GBC_NOTICE([%%% $pysite])
        AC_GBC_NOTICE([%%% inserted at head of PYTHONPATH])
    fi
fi
AC_ARG_VAR(PYTHONPATH, [as Python may require])

# however with 3.12 it seems pip is required for install
# on the other hand autoconf supports python installs...
PYTHON_SETUP="distutils.core"
AC_PATH_PROG(PIP, [pip], [have_pip=true] [have_pip=false])
if [test $have_python = "true"] ; then
    AC_GBC_NOTICE([%%% have_pip is $have_pip and it is $PIP])
    pyss="setuptools"
    pysetup=`$PYTHON -c "from $pyss import setup" 2>/dev/null ; echo $?`
    pyss="distutils.core"
    pydistc=`$PYTHON -c "from $pyss import setup" 2>/dev/null ; echo $?`
    if [ test x$pysetup$pydistc = 'x00' ] ; then
        PYTHON_SETUP="setuptools"
        AC_GBC_NOTICE([Apparently we have both setuptools or distutils.core])
    elif [ test x$pysetup$pydistc = 'x01' ] ; then
        PYTHON_SETUP="setuptools"
        AC_GBC_NOTICE([Apparently we have setuptools and not distutils.core])
    elif [ test x$pysetup$pydistc = 'x10' ] ; then
        PYTHON_SETUP="distutils.core"
        AC_GBC_NOTICE([Apparently we have distutils.core and not setuptools])
    elif [ test x$pysetup$pydistc = 'x11' ] ; then
        AC_GBC_NOTICE([Apparently we no distutils.core or setuptools])
    else
        AC_GBC_NOTICE([Apparently we are in a world of python hurt.])
    fi
    AC_GBC_NOTICE([%%% x.pysetup.pydistc is x$pysetup$pydistc])
fi
#declare that @PYTHON_SETUP@ will get substituted into files upon configure
AC_SUBST(PYTHON_SETUP)

# finally, the egg mechanism (pre-whl, pip) does not work in 3.11 if
# the install location is nonstandard; the fix is to unzip the eggs.
if [test $PYTHON_MAJOR = 3] ; then
    if [test -z $PYTHON_UNZIP_EGGS] ; then
        PYTHON_UNZIP_EGGS=true
        AC_MSG_NOTICE([
            Unzipping python eggs for chops.  You may adjust this
            behavior by setting PYTHON_UNZIP_EGGS true or false.
        ])
    fi
else
    if [test -z $PYTHON_UNZIP_EGGS] ; then
        PYTHON_UNZIP_EGGS=false
    fi
fi
AC_GBC_NOTICE([PYTHON_UNZIP_EGGS is $PYTHON_UNZIP_EGGS])
AC_ARG_VAR(PYTHON_UNZIP_EGGS, [if true unzip installed chops eggs in Python3])

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
# obsolescent: ISO C90 systems must have these:
# AC_HEADER_STDC

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
            AC_GBC_NOTICE([$pgplib has a png driver code])
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
if [test -n "$PGPLOT_FONT" ] ; then
    AC_GBC_NOTICE([PGP FONT is `ls -l $PGPLOT_FONT 2>&1`])
fi
PGPLOT_RGB=$rgbtxt
if [test -n "$PGPLOT_RGB" ] ; then
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
AC_GBC_LIBDEP(MSG,sub/msg,msg)
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
if [test x$build_vendor = xapple ] ; then
    LDFLAGS="-all_load $LDFLAGS"
fi

have_ompi=false
### option disabled
AC_SUBST(OMPIDIR)
AM_CONDITIONAL(HAVE_OMPI, [test $have_ompi = true])

needifxlibs=false
needifx=optional
### option disabled
AC_SUBST(D2M4DIR)
AM_CONDITIONAL(HAVE_D2M4, [$have_d2m4])

have_difx=false
#
# DiFX support, coercible via PKG_CONFIG_PATH
# The PKG_CONFIG_PATH setup is also used by mark5 (if used)
# If it has been set, it takes precedence to what we do here.
#
AC_ARG_VAR(PKG_CONFIG_PATH, [Package configuration path])
if [$needifxlibs] ; then
    if [test -n "$DIFXROOT"] ; then
        MK5_STUB=$srcdir/mark5/stubs
        if [test -n "$PKG_CONFIG_PATH" ] ; then
            PKG_CONFIG_PATH=${PKG_CONFIG_PATH}:$DIFXROOT/lib/pkgconfig
        else
            PKG_CONFIG_PATH=$DIFXROOT/lib/pkgconfig
        fi
        if [test -d "$MK5_STUB" ] ; then
            PKG_CONFIG_PATH=${PKG_CONFIG_PATH}:${MK5_STUB}
        fi
        export PKG_CONFIG_PATH
        if [test -d "$DIFXROOT/lib" ] ; then
            DIFXLIBS=$DIFXROOT/lib
        fi
    fi
    # DiFX 2.5.x -> 3.5.0, 2.6.x -> 3.6.0, 2.7++ -> 3.7.0
    PKG_CHECK_MODULES([DIFX],[difxio >= 3.7],
        [have_difx=true],[have_difx=false])
    if [test -z "$DIFXLIBS" -a -n "$DIFX_LIBS"] ; then
        DIFXLIBS=`echo $DIFX_LIBS | cut -d' ' -f1 | sed s/-L//`
    fi
    if [test -z "$DIFXLIBS" -a -z "$DIFX_LIBS" ] ; then
        AC_GBC_NOTICE([$DIFX_PKG_ERRORS])
        AC_GBC_NOTICE([Try defining DIFXROOT or PKG_CONFIG_PATH])
    fi
    DIFXDIR=difx
else
    have_difx=false
    MK5_STUB=''
    DIFXLIBS=''
    DIFXDIR=''
fi
AC_GBC_NOTICE([%%% have_difx is $have_difx ($needifx)])
AC_SUBST(DIFXLIBS)
AC_SUBST(DIFXDIR)
AM_CONDITIONAL(HAVE_DIFX, [$have_difx])

have_mark5=false
### option disabled
AC_SUBST(MARK5DIR)
AM_CONDITIONAL(HAVE_MARK5, $have_mark5)

have_docs=false
have_doxy=false
DOCSDIR=''
### option disabled
AC_SUBST(DOCSDIR)
AM_CONDITIONAL(HAVE_DOCS, [test $have_docs = true])
AM_CONDITIONAL(HAVE_DOXY, [test $have_doxy = true])

##############################################################################
##############################################################################

#
# Do you want to live on the edge?
# Use this to bypass explicit calls to AC_MSG_ERROR([cannot...], N)
#
if [test x$enable_png12 = xyes]; then
    pngrequired=required
else
    pngrequired=donotcare
fi
if [test x$enable_checks = xyes] ; then
  if [test x$have_hops = xtrue] ; then
    if [test x$have_gs = xfalse] ; then
        AC_MSG_ERROR([halted because of GHOSTSCRIPT issues], [1])
    elif [test x$have_x11 = xfalse] ; then
        AC_MSG_ERROR([halted because of X11 issues], [2])
    elif [test x$have_pgplot = xfalse] ; then
        AC_MSG_ERROR([halted because of PGPLOT issues], [3])
    elif [test x$hasfftw = xfalse] ; then
        AC_MSG_ERROR([halted because FFTW is missing], [4])
    elif [test x$have_png = xfalse -a x$need_png = x$pngrequired] ; then
        AC_MSG_ERROR([halted because of PNG issues (try --disable-png12)], [5])
    elif [test x$needifx = true -a x$have_difx = false] ; then
        AC_MSG_ERROR([halted because DIFX not properly found], [6])
    else
        gbc_banner='Configure passes sanity checks.'
    fi
  else
    gbc_banner='Sanity checks disabled.'
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
if [test "$have_hops" = true] ; then
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
        postproc/average/Makefile
        postproc/cohfit/Makefile
        postproc/fourfit/Makefile
        postproc/fourmer/Makefile
        postproc/fplot/Makefile
        postproc/fringex/Makefile
        postproc/search/Makefile
        postproc/snratio/Makefile
        sub/Makefile
        sub/msg/Makefile
        sub/afio/Makefile
        sub/dfio/Makefile
        sub/dfio/copypage/Makefile
        sub/util/Makefile
        sub/vex/Makefile
    ])
    AC_CONFIG_FILES(
        [data/ff_testdata/hopstest.py],
        [chmod +x data/ff_testdata/hopstest.py]
    )
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
    AC_CONFIG_FILES(
        [scripts/alma-vex-defs.py],
        [chmod +x scripts/alma-vex-defs.py]
    )
    AC_CONFIG_FILES(
        [scripts/hops_vex2ovex.py],
        [chmod +x scripts/hops_vex2ovex.py]
    )
    if [test $have_python = "true"] ; then
        AC_CONFIG_FILES([data/ff_testdata/hopstestsuite.py])
        AC_CONFIG_FILES([
            sub/mk4py/Makefile
            sub/mk4py/mk4.py
            sub/mk4py/difxio.py
            sub/mk4py/afio.py
        ])
        AC_CONFIG_FILES([sub/importcheck.py],
            [mv sub/importcheck.py sub/importcheck.py-;         dnl
             sed '/^personality/s/undefined/sub/'               dnl
                sub/importcheck.py- > sub/importcheck.py;       dnl
                rm sub/importcheck.py-;                         dnl
                chmod +x sub/importcheck.py])
    fi
fi

# this was under development, but is now available by default
if [test "$have_vex2xml" = true] ; then
    AC_CONFIG_FILES([vex2xml/Makefile])
fi

### option disabled

# d2m4 and ompi are handled by their subconfiguration

### option disabled

#
# And any HOP(Enhanced)S toys.
# aka CHOPS (chopped up HOPS).
#
if [test "$have_chops" = true] ; then
    AC_CONFIG_FILES([
        chops/Makefile
        chops/source/Makefile
        chops/source/c_src/Makefile
        chops/source/c_src/msg/Makefile
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
        chops/source/python_src/Makefile
        chops/source/python_src/scripts/Makefile
        chops/source/python_src/tests/Makefile
        chops/source/python_src/hopstest_module/Makefile
        chops/source/python_src/vpal_module/Makefile
    ])
    AC_CONFIG_FILES(
        [chops/hops_checksum.sh], [chmod +x chops/hops_checksum.sh])
    AC_CONFIG_FILES(
        [chops/run_checksum.sh], [chmod +x chops/run_checksum.sh])
    AC_CONFIG_FILES(
        [chops/importcheck.py],
        [mv chops/importcheck.py chops/importcheck.py-;                 dnl
         sed '/^personality/s/undefined/chops/' chops/importcheck.py- > dnl
            chops/importcheck.py; rm chops/importcheck.py-;             dnl
            chmod +x chops/importcheck.py])
    AC_CONFIG_FILES(
        [chops/source/python_src/aedit_gui/alist_test.py],
        [chmod +x chops/source/python_src/aedit_gui/alist_test.py])

    # The edits here makes the includes optional; dnl for readability
    # Some verions of automake/libtool get confused and put the
    # dependencies in the source directory in a way where they then
    # cannot be found in all build situations.  These dependencies are
    # really only an issue for the developer during development, anyway.
    AC_CONFIG_FILES(
        [chops/source/python_src/mk4_module/Makefile],
        [mv chops/source/python_src/mk4_module/Makefile      dnl
         chops/source/python_src/mk4_module/Makefile- ;      dnl
         sed 's/^include src/-include src/'                  dnl
             chops/source/python_src/mk4_module/Makefile-    dnl
             > chops/source/python_src/mk4_module/Makefile ; dnl
         rm chops/source/python_src/mk4_module/Makefile-
    ])
    AC_CONFIG_FILES(
        [chops/source/python_src/ffcontrol_module/Makefile],
        [mv chops/source/python_src/ffcontrol_module/Makefile     dnl
         chops/source/python_src/ffcontrol_module/Makefile- ;     dnl
         sed 's/^include src/-include src/'                       dnl
            chops/source/python_src/ffcontrol_module/Makefile-    dnl
            > chops/source/python_src/ffcontrol_module/Makefile ; dnl
         rm chops/source/python_src/ffcontrol_module/Makefile-
    ])
    AC_CONFIG_FILES(
        [chops/source/python_src/vex_module/Makefile],
        [mv chops/source/python_src/vex_module/Makefile      dnl
         chops/source/python_src/vex_module/Makefile- ;      dnl
         sed 's/^include src/-include src/'                  dnl
             chops/source/python_src/vex_module/Makefile-    dnl
             > chops/source/python_src/vex_module/Makefile ; dnl
         rm chops/source/python_src/vex_module/Makefile-
    ])

    # formally, the following may be skipped if we have no python
    if [test "$have_python" = true] ; then
        AC_CONFIG_FILES([
            chops/source/python_src/mk4_module/setup.py
            chops/source/python_src/mk4_module/afiob/afiob.py
            chops/source/python_src/mk4_module/mk4b/mk4b.py
            chops/source/python_src/hopstest_module/setup.py
            chops/source/python_src/hopstest_module/hopstestsuiteb.py
            chops/source/python_src/hopstest_module/hopstestb/hopstestb.py
            chops/source/python_src/ffcontrol_module/setup.py
            chops/source/python_src/ffcontrol_module/ffcontrol/ffcontrol.py
            chops/source/python_src/vex_module/setup.py
            chops/source/python_src/vex_module/vexpy/vexpy.py
            chops/source/python_src/vpal_module/setup.py
            chops/source/python_src/vpal_module/vpal/ffres2pcp_lib.py
            chops/source/python_src/vpal_module/vpal/fourphase_lib.py
            chops/source/python_src/vpal_module/vpal/fringe_file_manipulation.py
            chops/source/python_src/vpal_module/vpal/pcc_delay_fitting.py
            chops/source/python_src/vpal_module/vpal/pcc_plotting_utils.py
            chops/source/python_src/vpal_module/vpal/proxy_cable_cal.py
            chops/source/python_src/vpal_module/vpal/utility.py
            chops/source/python_src/vpal_module/vpal/processing.py
            chops/source/python_src/scripts/pcc_generate.py
            chops/source/python_src/scripts/parallactic_plots.py
            chops/source/python_src/scripts/az_el_amp_plot.py
            chops/source/python_src/scripts/compare_pol_hands.py
            chops/source/python_src/scripts/pcc_select.py
            chops/source/python_src/scripts/pcc_plot.py
            chops/source/python_src/scripts/vgoscf_generate.py
            chops/source/python_src/scripts/phase_resid.py
            chops/source/python_src/scripts/phase_resid_mixedmode.py
            chops/source/python_src/scripts/get_ionex_dtec_bounds.py
            chops/source/python_src/scripts/channel_phase_resid.py
            chops/source/python_src/scripts/channel_amplitude_compare.py
            chops/source/python_src/scripts/channel_pcal_amp.py
            chops/source/python_src/scripts/batch_fourfit.py
            chops/source/python_src/scripts/ffres2pcp.py
            chops/source/python_src/scripts/fourphase.py
            chops/source/python_src/scripts/batch_fourfit_mixedmode.py
            chops/source/python_src/scripts/ffres2pcp_mixedmode.py
            chops/source/python_src/scripts/fourphase_mixedmode.py
            chops/source/python_src/scripts/summarize_report.py
            chops/source/python_src/scripts/select_bandpols.py
            chops/source/python_src/scripts/pcal_amp_over_time.py
            chops/source/python_src/scripts/plot_ddTEC_minsnr.py
        ])
        AC_CONFIG_FILES(
            [chops/source/python_src/tests/test_harness.sh],
            [chmod +x chops/source/python_src/tests/test_harness.sh]
        )
        # the following perhaps do not need to be executable,
        # however they do all have __name__ == '__main__' constructs:
        AC_CONFIG_FILES(
            [chops/source/python_src/tests/test_pcc_generate.py],
            [chmod +x chops/source/python_src/tests/test_pcc_generate.py]
        )
        AC_CONFIG_FILES(
            [chops/source/python_src/tests/test_fourphase.py],
            [chmod +x chops/source/python_src/tests/test_fourphase.py]
        )
        AC_CONFIG_FILES(
            [chops/source/python_src/tests/test_mk4b.py],
            [chmod +x chops/source/python_src/tests/test_mk4b.py]
        )
        AC_CONFIG_FILES(
            [chops/source/python_src/tests/test_ffres2pcp.py],
            [chmod +x chops/source/python_src/tests/test_ffres2pcp.py]
        )
    fi
fi

#
# Shell setup convenience
#
AC_CONFIG_FILES(hops.bash, [chmod +x hops.bash])

# this creates and launches config.status that does the work
AC_OUTPUT

# finally the summary and setup instructions
# which unfortunately will follow AC_OUTPUT
# no matter what we do
AC_GBC_BANNER([$gbc_banner])
AC_GBC_BASHER([hops.bash],[$HOPS_ROOT/$HOPS_ARCH/bin])

#
# eof
#
