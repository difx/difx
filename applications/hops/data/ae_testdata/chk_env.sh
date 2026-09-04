#!/bin/bash
#
# $Id: chk_env.sh 4575 2026-05-21 21:31:28Z gbc $
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
    echo path set up using $hops
    true
} || {
    echo path set up locally in path
    rm -rf ./path
    mkdir ./path
    cd ./path
    for e in adump aedit alist
    do
	[ -x ../../../postproc/$e/$e ] && ln -s ../../../postproc/$e/$e .
    done
    for t in blk_stmt.txt  pformat.txt
    do
	[ -f ../../../../sub/vex/text/$t ] &&
	    ln -s ../../../../sub/vex/text/$t .
    done
    export PATH=`pwd`:$PATH
    [ -z "$LD_LIBRARY_PATH" ] && LD_LIBRARY_PATH=/dev/null
    export LD_LIBRARY_PATH LD_LIBRARY_PATH=$PGPLOT_DIR:$LD_LIBRARY_PATH
    export DEF_CONTROL=/dev/null
    export TEXT=`pwd`
    export AHELP=$srcdir/../../help/aedit
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
echo AHELP is $AHELP

#
# eof
#
