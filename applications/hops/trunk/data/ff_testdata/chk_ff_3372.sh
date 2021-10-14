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
[ -f ./ff-3372.ps ] || { echo ./ff-3372.ps missing && exit 2 ; }

# pluck out lines containing the snr and amp, parse and check

# ff-3372.ps:7570 9384 M (21.078) SR
line=$(grep '7570 9384' ./ff-3372.ps)
IFS='()'
read a amp b <<<"$line"
low=21.058
high=21.088
okamp=$(echo "$amp>$low && $amp<$high" | bc)
$verb && echo okamp is $okamp and "$low < $amp < $high" is expected from: $line
# ff-2836.ps:7570 9653 M (144.1) SR
lsnr=$(grep '7570 9653' ./ff-3372.ps)
IFS='()'
read a snr b <<<"$lsnr"
low=144.0
high=144.2
oksnr=$(echo "$snr>$low && $snr<$high" | bc)
$verb && echo oksnr is $oksnr and "$low < $snr < $high" is expected from: $lsnr
#
[ "$okamp" -gt 0 -a "$oksnr" -gt 0 ]

#
# eof
#
