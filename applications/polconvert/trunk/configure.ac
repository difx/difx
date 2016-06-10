#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
# autoconf config file for building PolConvert within DiFX tree.
#
AC_PREREQ([2.69])
AC_INIT([src/DataIO.h],[1.2],[Ivan Marti-Vidal <ivan.marti-vidal@chalmers.se>])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])

# the assumption here is that either CASA is already
# in PATH or CASAPATH=casa-release-.../bin is defined
# with the latter taking precedence.
AC_PATH_PROG([CASACMD], [casa], [/bin/false], [PATH = ${CASAPATH}])
if [test x$CASACMD = /bin/false] ; then
    AC_PATH_PROG([CASACMD], [casa], [/bin/false])
    if [test x$CASACMD = /bin/false] ; then
        have_casa=false
        AC_MSG_NOTICE(casa was not found in path)
    else
        AC_MSG_NOTICE([CASACMD is $CASACMD])
        have_casa=true
    fi
    have_casa=false
    AC_MSG_NOTICE(casa was not found in path)
else
    have_casa=true
fi
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
AC_CHECK_HEADERS([fitsio.h stdlib.h string.h Python.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL

# Checks for library functions.
AC_FUNC_REALLOC
AC_CHECK_FUNCS([sqrt])

# PolConvert links with cfitsio
AC_CHECK_LIB(cfitsio, fits_open_file, have_fits=true, have_fits=false)
AM_CONDITIONAL(HAVE_FITS, $have_fits)

# no need for the surgical conditionals at this time
if [test have_casa = true -a have_java = true -a have_fits = true] ; then
    have_it_all=true
else
    have_it_all=false
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
