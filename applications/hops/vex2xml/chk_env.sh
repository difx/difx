#!/bin/sh
#
# $Id: chk_env.sh 1853 2017-07-08 15:21:38Z gbc $
#
hops=../hops.bash

verb=false
[ -n "$testverb" ] && verb=true && HOPS_QUIET='' || HOPS_QUIET=true

$verb && echo srcdir is $srcdir
umask 0002

[ -x $hops ] && {
    . $hops
    $verb && echo path set up using $hops &&
        ls -l $hops
}

[ -f $HOPS_ROOT/$HOPS_ARCH/lib/hops/VEX2XML.jar ] || {
    rm -rf jcl
    mkdir -p jcl/lib/hops

    cp -p $srcdir/lib/*jar jcl/lib/hops
    cp -p VEX2XML.jar jcl/lib/hops

    export HOPS_ROOT=.
    export HOPS_ARCH=jcl
    $verb && echo now using $HOPS_ROOT/$HOPS_ARCH &&
        ls -lR $HOPS_ROOT/$HOPS_ARCH
    true
}

[ -x VEX2XML ] || {
    [ -d jcl ] | mkdir jcl
    cp $srcdir/VEX2XML ./jcl
    chmod +x ./jcl/VEX2XML
    export PATH=./jcl:$PATH
    $verb && echo added VEX2XML into jcl: $PATH
}

#
# eof
#
