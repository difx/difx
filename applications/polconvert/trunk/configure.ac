#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# autoconf config file for building PolConvert within DiFX tree.
#
AC_PREREQ([2.63])
AC_INIT([polconvert],[1.7],[Ivan Marti-Vidal <ivan.marti-vidal@chalmers.se>])
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
    if [test x$CASACMD = /bin/false] ; then
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
fi
#AC_SUBST(DIFXCASAPATH)
AM_CONDITIONAL(HAVE_CASA, $have_casa)

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
