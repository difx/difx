#
# Process this file with autoconf to produce a configure script.
# Run autoscan to get suggestions on more things to check.
#
# Note that HOPS doesn't presently use hops_config.h except
# to detect endianism in bytflp.h for some of the tools.
#
AC_PREREQ(2.59)
AC_INIT(hops, 3.7, gbc@haystack.mit.edu)
#AM_INIT_AUTOMAKE([foreign std-options])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_SRCDIR([hops.bash.in])
AC_CONFIG_HEADER([hops_config.h])

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
#
AC_ARG_ENABLE(hops,
    AS_HELP_STRING([--disable-hops], [disables the post-processing tools]),
    [], [enable_hops=yes])
AC_GBC_NOTICE([enable_hops is --$enable_hops--])
### option disabled
AC_ARG_ENABLE(pathx,
    AS_HELP_STRING([--enable-pathx], [try normal autoconf x11 path checks]))
AC_GBC_NOTICE([enable_pathx is --$enable_pathx--])
AC_ARG_ENABLE(xtrap,
    AS_HELP_STRING([--enable-xtrap], [try extra autoconf x11 path checks]))
AC_GBC_NOTICE([enable_xtrap is --$enable_xtrap--])
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

have_devel=false
### option disabled
AC_SUBST(DEVLDIR)
AC_SUBST(PROGDIR)
AC_SUBST(IFFYDIR)
AM_CONDITIONAL(HAVE_DEVEL, $have_devel)

# autoscan suggested list of checks follows

# Checks for programs.
AC_PROG_CC
AC_PROG_RANLIB
AC_PROG_F77
AC_F77_LIBRARY_LDFLAGS
# this must follow AC_PROG_CC and AC_PROG_F77 for some reason
if [test x$enable_mark5 = xyes] ; then
    AC_PROG_CXX
else
    # define the conditionals AC_PROG_CXX normally does
    am__fastdepCXX_TRUE='#'
    am__fastdepCXX_FALSE='#'
fi
# HOPS is expecting /usr/bin/gs to exist
AC_CHECK_PROG(HAVE_GS, [gs], [true], [false], [/usr/bin])
AC_GBC_NOTICE([%%% have_gs is $HAVE_GS (required)])

# PERL/Python might be in nonstandard locations
AC_PATH_PROGS(PERL, perl, /bin/false)
AC_PATH_PROGS(PYTHON, python, /bin/false)

# Checks for libraries.
AC_CHECK_LIB(m, floorl)

# Checks for header files.
if [$ac_hops] ; then
AC_FUNC_ALLOCA
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_CHECK_HEADERS([arpa/inet.h fcntl.h float.h inttypes.h limits.h])
AC_CHECK_HEADERS([locale.h netdb.h netinet/in.h stddef.h stdint.h])
AC_CHECK_HEADERS([stdlib.h string.h strings.h sys/socket.h unistd.h])
fi

# Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN(
    AC_DEFINE(WORDS_BIGENDIAN, 1, [this is a BigEndian Host]),
    AC_DEFINE(WORDS_BIGENDIAN, 0, [this is a LittleEndian Host])
)

# additional items for hops_config
svnexec=`type -p svn`
if [test -d $srcdir/.svn -a -n "$svnexec" -a -x "$svnexec"] ; then
    # only available if svn is the source and svn is installed
    hops_svn_rev=`(cd $srcdir ; svn info . ) | grep Revision | cut -d' ' -f2`
elif [test -f $srcdir/svnrev.txt] ; then
    # provided by disthook for tarballs
    hops_svn_rev=`cat $srcdir/svnrev.txt`
elif [test -f $srcdir/config.svnrev.txt] ; then
    # provided by nightly for client builds
    hops_svn_rev=`cat $srcdir/config.svnrev.txt`
else
    hops_svn_rev=0
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
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_C_VOLATILE
fi

# Checks for library functions.
if [$ac_hops] ; then
AC_FUNC_CLOSEDIR_VOID
AC_FUNC_LSTAT
AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
AC_FUNC_MALLOC
AC_FUNC_MEMCMP
AC_FUNC_REALLOC
AC_FUNC_STAT
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([floor getcwd isascii memset mkdir modf pow rint sqrt])
AC_CHECK_FUNCS([strcasecmp strchr strdup strerror strncasecmp strrchr])
AC_CHECK_FUNCS([strspn strstr])
fi

have_gsl=false
### option disabled
AM_CONDITIONAL(HAVE_GSL, $have_gsl)
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
AC_ARG_VAR(PGPLOT_DIR, [PGPLOT installation directory])
if [test x${enable_pgplot-yes} = xyes ] ; then
    AC_GBC_NOTICE([searching for pgxwin_server])
    AC_PATH_PROG(PGXWIN_SERVER, pgxwin_server, false,
	[${PGPLOT_DIR-'/usr/local/pgplot'} /usr/lib/pgplot5 dnl
	 /usr/lib/pgplot64 /usr/local/pgplot-png dnl
	 /usr/bin /opt/local/bin /sw/lib/pgplot])
    pgpdir=`AS_DIRNAME(["${PGXWIN_SERVER-/no-such-dir}"])`
    if [test "$pgpdir" = '/usr/bin'] ; then
	# for Debian packages
	pgpdir='/usr/lib'
	pgplib=$pgpdir/libpgplot.a
	cpglib=$pgpdir/libcpgplot.a
	grfont=${PGPLOT_DIR-'/usr/lib/pgplot5'}/grfont.dat
	AC_GBC_NOTICE([pgpdir was /usr/bin, revised to $pgpdir])
    elif [test "$pgpdir" = '/opt/local/bin'] ; then
	# for MacPorts
	pgpdir='/opt/local/lib'
	pgplib=$pgpdir/libpgplot.a
	cpglib=$pgpdir/libcpgplot.a
	grfont=/opt/local/share/pgplot/grfont.dat
	AC_GBC_NOTICE([pgpdir was /opt/local, revised to $pgpdir])
    else
	# well, no pgxwin_server, but PGPLOT may yet be functional
	if [test "$pgpdir" = '/no-such-dir'] ; then
	    pgpdir=${PGPLOT_DIR='/usr/local/pgplot'}
	fi
	pgplib=$pgpdir/libpgplot.a
	cpglib=$pgpdir/libcpgplot.a
	grfont=$pgpdir/grfont.dat
	AC_GBC_NOTICE([pgpdir is $pgpdir])
    fi
    if [test -d $pgpdir -a -f $pgplib -a -f $cpglib -a $grfont ] ; then
	AC_GBC_NOTICE([PGPLOT_DIR is $pgpdir])
	have_pgplot=true
	PGPLOT_DIR=$pgpdir
	# PGP_LIB=['$(FLIBS) -L$(PGPLOT_DIR) -lcpgplot -lpgplot']
	PGP_LIB=['-L$(PGPLOT_DIR) -lcpgplot -lpgplot $(FLIBS)']
    else
	AC_GBC_NOTICE([PGPLOT was too hard to find.])
	AC_GBC_NOTICE([Try setting PGPLOT_DIR to point to a directory])
	AC_GBC_NOTICE([that contains symbolic links to the installed])
	AC_GBC_NOTICE([files libpgplot.a, libcpgplot.a and grfont.dat,])
	AC_GBC_NOTICE([and also make sure pgxwin_server is on your PATH.])
	have_pgplot=false
	PGPLOT_DIR=''
	PGP_LIB=''
    fi
    # does this PGPLOT compilation require PNG(12)?
    need_png=optional
    if [test -f $pgplib] ; then
	pndriv=`ar t $pgplib pndriv.o`
	if [test x$pndriv = xpndriv.o] ; then
	    need_png=required
	fi
    fi
fi
AC_GBC_NOTICE([%%% have_pgplot is $have_pgplot (required)])
AM_CONDITIONAL(HAVE_PGPLOT, [test x$have_pgplot = xtrue])
AC_SUBST(PGP_LIB)
AC_SUBST(PGPLOT_DIR)

# Check for PNG (primitive) which MAY be required by pgplot
# One could provide the linkage stubs if PNG is required, but missing.
AC_ARG_VAR(PNG12_LIB, [Path to libpng12 shared object])
AC_CHECK_LIB(png12, png_access_version_number, have_png=true, have_png=false)
if [$have_png] ; then
    AC_GBC_NOTICE([Found png12 normally])
    PNG_LIB='-lpng12'
else
    AC_GBC_NOTICE([Looking harder for png12])
    if [test -z "$PNG12_LIB" ]; then
	PNG12_LIB=${DIFX_ARCH_ROOT}/lib/libpng12.so.0
    elif [test -z "$PNG12_LIB" ]; then
	PNG12_LIB=/usr/lib/libpng12.so.0
    fi
    if [test -f "$PNG12_LIB" ] ; then
	have_png=true
	PNG_LIB=$PNG12_LIB
    else
	have_png=false
	PNG_LIB=''
    fi
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
    elif [test x$have_png = xfalse] ; then
	AC_MSG_ERROR([halted because of PNG issues], 4)
    else
	AC_GBC_BANNER([Configure passes sanity checks.])
    fi
  else
    AC_GBC_BANNER([Sanity checks disabled.])
  fi
fi

#
# Always make these files....
#
AC_CONFIG_FILES([
    Makefile
    README.txt
])

#
# The core HOPS postprocessing tools
#
if [ $have_hops = "true" ] ; then
    AC_CONFIG_FILES([
	help/Makefile
	include/Makefile
	scripts/Makefile
	correlator/Makefile
	correlator/CorAsc2/Makefile
	data/Makefile
	data/ff_testdata/Makefile
	postproc/Makefile
	postproc/adump/Makefile
	postproc/aedit/Makefile
	postproc/alist/Makefile
	postproc/fourfit/Makefile
	postproc/fourmer/Makefile
	postproc/fplot/Makefile
	postproc/fringex/Makefile
	sub/Makefile
	sub/afio/Makefile
	sub/dfio/Makefile
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
fi

### option disabled

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
