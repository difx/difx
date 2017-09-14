#!/bin/sh
#
# $Id: chk_env.sh 1917 2017-07-28 15:59:11Z barrettj $
#
# environment setup -- HOPS_SETUP is not set or false
# This script is really only needed for distcheck where
# the normal setup doesn't work correctly (yet).
#
# ${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
#

hops=../../hops.bash
tste=$bindir/env_check
verb=false
[ -n "$testverb" ] && verb=true && HOPS_QUIET='' || HOPS_QUIET=true

$verb &&
    echo srcdir is $srcdir &&
    echo bindir is $bindir

# normal setup
umask 0002
[ -n "$bindir" -a -d "$bindir" -a -x "$tste" -a -x $hops ] && {
    . $hops
    $verb && echo path set up using $hops
    true
} || {
    rm -rf ./path
    mkdir ./path
    cd ./path
    for e in adump aedit alist average bispec calamp cofit \
	     coterp fold fourfit fourmer fplot fringex \
	     pratio search
    do
	[ -x ../../../postproc/$e/$e ] && ln -s ../../../postproc/$e/$e .
    done
    for e in hops_data_links.pl
    do
	[ -x ../../../scripts/$e ] && ln -s ../../../scripts/$e .
    done
    for t in blk_stmt.txt  pformat.txt
    do
	[ -f ../../../../sub/vex/text/$t ] &&
	    ln -s ../../../../sub/vex/text/$t .
    done
    export PATH=`pwd`:$PATH
    export TEMP_MK4PY_LIB_DIR=`pwd`/../../../sub/mk4py/.libs
    [ -z "$LD_LIBRARY_PATH" ] && LD_LIBRARY_PATH=/dev/null
    export LD_LIBRARY_PATH LD_LIBRARY_PATH=$PGPLOT_DIR:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH LD_LIBRARY_PATH=$TEMP_MK4PY_LIB_DIR:$LD_LIBRARY_PATH
    export DEF_CONTROL=/dev/null
    export TEXT=`pwd`
    export HOPS_PYTEST_SETUP_NEEDED='true'
    $verb &&
	echo made links in `pwd` &&
	echo TEXT is $TEXT &&
	echo PATH is $PATH &&
	echo LD_LIBRARY_PATH is $LD_LIBRARY_PATH &&
	echo DEF_CONTROL is /dev/null &&
	echo "bindir ($bindir) tste ($tste) hops ($hops)" &&
	echo ''
    cd ..
}

#
# eof
#
