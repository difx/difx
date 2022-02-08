#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# autoconf config file for building PolConvert within DiFX tree.
#
AC_PREREQ([2.63])
AC_INIT([polconvert],[1.8],
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
    # Version 6.x casa-release6/bin/python3 -> casa
    # Version 5.x casa-release5/bin/python -> casa
    AC_PATH_PROG([casapy3], [python3], [/bin/false], [$DIFXCASAPATH])
    AC_PATH_PROG([casapy2], [python], [/bin/false], [$DIFXCASAPATH])
    # we presume only one will be defined.  See the Makefile.am
    # to understand what is going on with BMT*
    if [test x$casapy3 = x$DIFXCASAPATH/python3] ; then
        BMTASKARG='--upgrade polconvert.xml'
        BMTASKXML='polconvert.xml'
        CASAPYTHON=$casapy3
        AC_MSG_NOTICE([Invoking CASAPYTHON as $CASAPYTHON])
    elif [test x$casapy2 = x$DIFXCASAPATH/python] ; then
        BMTASKARG='&& false'
        BMTASKXML='&& false'
        CASAPYTHON=$casapy2
        AC_MSG_NOTICE([Invoking CASAPYTHON as $CASAPYTHON])
    else
        AC_MSG_NOTICE([Did not find python within CASA])
        have_casa=false
    fi
fi
# try to use whatever is in CASA
DIFXCASALIBPATH=$DIFXCASAPATH/../lib
AC_SUBST(BMTASKXML)
AC_SUBST(BMTASKARG)
AC_SUBST(CASAPYTHON)
AC_SUBST(DIFXCASAPATH)
AC_SUBST(DIFXCASALIBPATH)
AM_CONDITIONAL(HAVE_CASA, $have_casa)
# until we figure out how to automatically write setup.py
# this environment variable may be needed
AC_MSG_NOTICE([POLGAINSOLVELIBS is $POLGAINSOLVELIBS])

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP

AC_PATH_PROG([JAVACMD], [java], [/bin/false])
if [test x$JAVACMD = /bin/false] ; then
    have_java=false
else
    have_java=true
fi
AM_CONDITIONAL(HAVE_JAVA, $have_java)

# Checks for header files.
CPPFLAGS="$CPPFLAGS -I/usr/include/cfitsio -I/usr/include/python2.7"
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
