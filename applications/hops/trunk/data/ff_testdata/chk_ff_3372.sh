#!/bin/sh
#
# $Id: chk_ff_3372.sh 870 2013-10-07 18:21:50Z rjc $
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
grep -v $os $DATADIR/3372/cf3372 > ./cf3372

$verb && echo \
fourfit -pt -b TV:X \\ && echo \
    -c ./cf3372 \\ && echo \
    $DATADIR/3372/193-1757/0529+483.vtqbsq

( echo sff-3372.ps; echo q ) | (
    fourfit -pt -b TV:X \
	-c ./cf3372 \
	$DATADIR/3372/193-1757/0529+483.vtqbsq
) 2>/dev/null 1>&2

# pluck out line containing the snr and parse it
line=$(grep '7570 9653' ./ff-3372.ps)

IFS='()'
read a snr b <<<"$line"

# snr bounds
low=143.6
high=144.6
aok=$(echo "$snr>$low && $snr<$high" | bc)

[ $aok -gt 0 ]

#
# eof
#
