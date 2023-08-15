#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# autoconf config file for building PolConvert within DiFX tree.
#
AC_PREREQ([2.63])
AC_INIT([polconvert],[2.0.7],
    [Ivan Marti-Vidal <ivan.marti-vidal@i.marti-vidal@uv.es>])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])

# the assumption here is that either CASA is already in PATH or
# else DIFXCASAPATH=casa-release-.../bin is defined (to be specific).
# The latter taking precedence.
AC_ARG_VAR([DIFXCASAPATH],[Path to CASA installation])
AC_MSG_NOTICE([DIFXCASAPATH is $DIFXCASAPATH])
AC_PATH_PROG([CASACMD], [casa], [/bin/false], [$DIFXCASAPATH])
if [test x$CASACMD = x/bin/false] ; then
    AC_MSG_NOTICE([casa was not found in DIFXCASAPATH, $DIFXCASAPATH])
    AC_PATH_PROG([CASACMD], [casa], [/bin/false])
    if [test x$CASACMD = x/bin/false] ; then
        have_casa=false
        AC_MSG_NOTICE(casa was not found in path, case 1)
    else
        have_casa=true
        DIFXCASAPATH=`AS_DIRNAME([$CASACMD])`
        AC_MSG_NOTICE([CASACMD is $CASACMD])
        AC_MSG_NOTICE([DIFXCASAPATH is $DIFXCASAPATH])
    fi
    have_casa=false
    AC_MSG_NOTICE(casa was not found in path, case 2)
else
    # DIFXCASAPATH usefully defined
    have_casa=true
    AC_MSG_NOTICE([CASACMD is $CASACMD])
    AC_MSG_NOTICE([DIFXCASAPATH is $DIFXCASAPATH])
fi
# additional test for buildmytasks in case user has cheated on CASA
if [test $have_casa = true] ; then
    AC_PATH_PROG([BUILDMYTASKS], [buildmytasks], [/bin/false], [$DIFXCASAPATH])
    if [test x$BUILDMYTASKS = x/bin/false] ; then
	have_casa=false
	AC_MSG_NOTICE(buildmytasks was not found in path, case 3)
    else
	AC_MSG_NOTICE([BUILDMYTASKS is $BUILDMYTASKS])
    fi
    # Version 6.x casa-release6/bin/python3 -> casa -- case 1
    # Version 5.x casa-release5/bin/python -> casa  -- case 2
    # neither                                       -- case 3
    AC_PATH_PROG([casapy3], [python3], [/bin/false], [$DIFXCASAPATH])
    AC_PATH_PROG([casapy2], [python], [/bin/false], [$DIFXCASAPATH])
    # we presume only one will be defined.  See the Makefile.am
    # to understand what is going on with BMT*
    if [test x$casapy3 = x$DIFXCASAPATH/python3] ; then
        BMTASKMOD='polconvertpkg'
        BMTASKXML='polconvert.xml'
        CASAPYTHON=$casapy3
        have_casa6=true
        AC_MSG_NOTICE([Invoking CASAPYTHON as $CASAPYTHON])
        casapythonincludedir=`ls -d $DIFXCASAPATH/../lib/py/python3.* 2>&-`
    elif [test x$casapy2 = x$DIFXCASAPATH/python] ; then
        BMTASKMOD=''
        BMTASKXML=''
        CASAPYTHON=$casapy2
        have_casa6=false
        AC_MSG_NOTICE([Invoking CASAPYTHON as $CASAPYTHON])
        casapythonincludedir=`ls -d $DIFXCASAPATH/../lib/python2.* 2>&-`
    else
        AC_MSG_NOTICE([Did not find python within CASA])
        CASAPYTHON=/bin/false
        have_casa=false
        have_casa6=false
    fi
fi
if [test -d $casapythonincludedir ] ; then
    AC_MSG_NOTICE([casapythonincludedir exists])
else
    casapythonincludedir=no-casa-python-include-directory
fi
AC_MSG_NOTICE([casapythonincludedir is $casapythonincludedir])
# try to use whatever is in CASA
DIFXCASALIBPATH=$DIFXCASAPATH/../lib
AC_SUBST(BMTASKXML)
AC_SUBST(BMTASKMOD)
AC_SUBST(CASAPYTHON)
AC_SUBST(DIFXCASAPATH)
AC_SUBST(DIFXCASALIBPATH)
AM_CONDITIONAL(HAVE_CASA, $have_casa)
AM_CONDITIONAL(HAVE_CASA6, $have_casa6)

# more variables in setup...
if [test -d /usr/include/cfitsio ] ; then
    POLCONVERTCFITSIODIR=/usr/include/cfitsio
    AC_MSG_NOTICE([POLCONVERTCFITSIODIR is $POLCONVERTCFITSIODIR])
else
    # not sure where else it might be these days
    AC_MSG_NOTICE([WARNING: you need to find include files for cfitsio])
fi
AC_SUBST(POLCONVERTCFITSIODIR)

# make DO_SOLVE controllable here
if [test x$POLCONVERTDOSOLVE = x] ; then
    POLCONVERTDOSOLVE=True
    AC_MSG_NOTICE([POLCONVERTDOSOLVE set to $POLCONVERTDOSOLVE])
else
    AC_MSG_NOTICE([POLCONVERTDOSOLVE is $POLCONVERTDOSOLVE])
fi
AC_SUBST(POLCONVERTDOSOLVE)

# until we figure out how to automatically write setup.py
# this environment variable may be needed
AC_MSG_NOTICE([POLGAINSOLVELIBS is initially $POLGAINSOLVELIBS])

# AC_CHECK_LIB could be used, but setup.py isn't set up for that...
# so, this is a pass at building the list the setup.py wants.
if [test x$POLGAINSOLVELIBS = x] ; then
    pcgsl=`ls $DIFXCASALIBPATH/libgsl.so.* | tail -1`
    if [test -f $pcgsl] ; then
        POLGAINSOLVELIBS=gsl
        AC_MSG_NOTICE([gsl is $pcgsl])
    fi
    pcgslcblas=`ls $DIFXCASALIBPATH/libgslcblas.so.* | tail -1`
    if [test -f $pcgslcblas] ; then
        if [test x$POLGAINSOLVELIBS = x] ; then comma='' ; else comma=',' ; fi
        POLGAINSOLVELIBS=$POLGAINSOLVELIBS${comma}gslcblas
        AC_MSG_NOTICE([gslcblas is $pcgslcblas])
    else
        pccblas=`ls $DIFXCASALIBPATH/libcblas.so.* | tail -1`
        if [test -f $pccblas] ; then
            if [test x$POLGAINSOLVELIBS = x] ; then comma='' ; else comma=',' ; fi
            POLGAINSOLVELIBS=$POLGAINSOLVELIBS${comma}cblas
            AC_MSG_NOTICE([cblas is $pcblas])
        fi
    fi
    pcfftw3=`ls $DIFXCASALIBPATH/libfftw3.so.* | tail -1`
    if [test -f $pcfftw3] ; then
        if [test x$POLGAINSOLVELIBS = x] ; then comma='' ; else comma=',' ; fi
        POLGAINSOLVELIBS=$POLGAINSOLVELIBS${comma}fftw3
        AC_MSG_NOTICE([fftw3 is $pcfftw3])
    fi
    AC_MSG_NOTICE([POLGAINSOLVELIBS is finally $POLGAINSOLVELIBS])
fi
AC_SUBST(POLGAINSOLVELIBS)

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP

# java is required for buildmytasks, e.g. casa-release/
# lib/py/lib/python3.8/site-packages/casatools/__casac__/bin/xml-casa
AC_PATH_PROG([JAVACMD], [java], [/bin/false])
if [test x$JAVACMD = /bin/false] ; then
    have_java=false
else
    have_java=true
fi
AM_CONDITIONAL(HAVE_JAVA, $have_java)

# Checks for header files.
CPPFLAGS="$CPPFLAGS -I$POLCONVERTCFITSIODIR -I$casapythonincludedir"
AC_CHECK_HEADERS([fitsio.h stdlib.h string.h Python.h])

# Checks for typedefs, structures, and compiler characteristics.
# AC_CHECK_HEADER_STDBOOL

# Checks for library functions.
AC_FUNC_REALLOC
AC_CHECK_FUNCS([sqrt])

# PolConvert links with cfitsio
AC_CHECK_LIB(cfitsio, ffopen, have_fits=true, have_fits=false)
AM_CONDITIONAL(HAVE_FITS, $have_fits)

# no need for the surgical conditionals at this time
AC_MSG_NOTICE([casa/java/fits are $have_casa/$have_java/$have_fits])
if [test $have_casa = true -a $have_java = true -a $have_fits = true] ; then
    have_it_all=true
    AC_MSG_NOTICE([We have prerequisites for polconvert])
else
    have_it_all=false
    AC_MSG_NOTICE([Missing required things for polconvert])
fi
AM_CONDITIONAL(HAVE_IT_ALL, $have_it_all)

AC_CONFIG_FILES([
    Makefile
    src/Makefile
])
AC_OUTPUT
#
# eof
#
