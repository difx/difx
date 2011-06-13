#!/bin/sh
#
# $Id: chk_ff_2836.sh 330 2011-06-10 13:32:10Z gbc $
#
# canonical test suite for fourfit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

[ -n "$DISPLAY" ] || { echo Skipping test--DISPLAY is undefined; exit 0; }

os=`uname -s` || os=idunno
grep -v $os $DATADIR/2836/cf2836 > ./cf2836

$verb && echo \
fourfit -pt -b AE:X \\ && echo \
    -c ./cf2836 \\ && echo \
    $DATADIR/2836/scan001/2145+067.olomfh

( echo sff-2836.ps; echo q ) | (
    fourfit -pt -b AE:X \
	-c ./cf2836 \
	$DATADIR/2836/scan001/2145+067.olomfh
) 2>/dev/null 1>&2

set -- `ls -s ff-2836.ps` 0 no-such-file
$verb && echo $@

# did we make a plot?
[ $1 -ge 76 ] && rm -f ./cf2836

#
# eof
#
