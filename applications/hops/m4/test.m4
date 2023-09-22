#
# $Id$
#
# AC_GBC_INIT([comment])
#-----------------------
AC_DEFUN([AC_GBC_INIT], [dnl
rm -f config.summary ; echo [$1] > config.summary
])

# AC_GBC_NOTICE([normal notice arguments])
#-----------------------------------------
AC_DEFUN([AC_GBC_NOTICE], [dnl
echo "[$1]" >> config.summary
AC_MSG_NOTICE([$1])dnl
])

# AC_GBC_BANNER([comment])
#-------------------------
AC_DEFUN([AC_GBC_BANNER], [dnl
    echo ''
    echo '  '[$1]
    echo ''
    grep '%%%' config.summary | sed 's/^.../    /'
    echo ''
])

# AC_GBC_LIBDEP([name],[dir],[lib])
#-------------------------
AC_DEFUN([AC_GBC_LIBDEP], [dnl
$1_LIB=['-L$(top_builddir)/$2 -l$3']
$1_DEP=['$(top_builddir)/$2/lib$3.a']
AC_SUBST($1_LIB)
AC_SUBST($1_DEP)
])

#
# eof
#
