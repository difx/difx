#!/bin/sh
#
# $Id: chk_ff_2843.sh 330 2011-06-10 13:32:10Z gbc $
#
# canonical test suite for fourfit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

[ -n "$DISPLAY" ] || { echo Skipping test--DISPLAY is undefined; exit 0; }

$verb && echo \
fourfit -pt -b AI \\ && echo \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak

# AIT
( echo sff-2843.ps; echo q ) | (
    fourfit -pt -b AI \
	$DATADIR/2843/321-1701_0552+398/0552+398.oifhak
) 2>/dev/null 1>&2

set -- `ls -s ff-2843.ps` 0 no-such-file
$verb && echo $@

# did we make a plot?
[ $1 -ge 74 ]

#
# eof
#
