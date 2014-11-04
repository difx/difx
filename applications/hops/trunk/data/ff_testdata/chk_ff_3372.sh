#!/bin/bash
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

os=`uname -s` || os=idunno
grep -v $os $DATADIR/3372/cf3372 > ./cf3372

rm -f ff-3372.ps
$verb && echo \
fourfit -t -d diskfile:ff-3372.ps -b TV:X \\ && echo \
    -c ./cf3372 \\ && echo \
    $DATADIR/3372/193-1757/0529+483.vtqbsq

fourfit -t -d diskfile:ff-3372.ps -b TV:X \
    -c ./cf3372 \
    $DATADIR/3372/193-1757/0529+483.vtqbsq 2>/dev/null 1>&2

# pluck out line containing the snr and parse it
line=$(grep '7570 9653' ./ff-3372.ps)

IFS='()'
read a snr b <<<"$line"

# snr bounds
low=143.6
high=144.6
aok=$(echo "$snr>$low && $snr<$high" | bc)
$verb && echo aok is $aok and "$low < $snr < $high" is expected from: $line

[ "$aok" -gt 0 ]

#
# eof
#
