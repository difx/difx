#!/bin/sh
#
# $Id: chk_env.sh 332 2011-06-10 18:06:52Z gbc $
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
    export PATH=`pwd`:$PATH
    [ -z "$LD_LIBRARY_PATH" ] && LD_LIBRARY_PATH=/dev/null
    export LD_LIBRARY_PATH LD_LIBRARY_PATH=$PGPLOT_DIR:$LD_LIBRARY_PATH
    export DEF_CONTROL=/dev/null
    $verb &&
	echo made links in `pwd` &&
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
