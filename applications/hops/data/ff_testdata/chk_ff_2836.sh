#!/bin/bash
#
# $Id: chk_ff_2836.sh 3327 2021-09-04 13:47:06Z gbc $
#
# canonical test suite for fourfit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

os=`uname -s` || os=idunno
grep -v $os $DATADIR/2836/cf2836 > ./cf2836

rm -f ff-2836.ps
$verb && echo \
fourfit -t -d diskfile:ff-2836.ps -b AE:X \\ && echo \
    -c ./cf2836 \\ && echo \
    $DATADIR/2836/scan001/2145+067.olomfh

fourfit -t -d diskfile:ff-2836.ps -b AE:X \
    -c ./cf2836 \
    $DATADIR/2836/scan001/2145+067.olomfh 2>/dev/null 1>&2
[ -f ./ff-2836.ps ] || { echo ./ff-2836.ps missing && exit 2 ; }

# pluck out lines containing the snr and amp, parse and check

# ff-2836.ps:7570 9384 M (26.254) SR
line=$(grep '7570 9384' ./ff-2836.ps)
IFS='()'
read a amp b <<<"$line"
low=26.244
high=26.264
okamp=$(echo "$amp>$low && $amp<$high" | bc)
$verb && echo okamp is $okamp and "$low < $amp < $high" is expected from: $line
# ff-2836.ps:7570 9653 M (140.4) SR
lsnr=$(grep '7570 9653' ./ff-2836.ps)
IFS='()'
read a snr b <<<"$lsnr"
low=140.0
high=140.8
oksnr=$(echo "$snr>$low && $snr<$high" | bc)
$verb && echo oksnr is $oksnr and "$low < $snr < $high" is expected from: $lsnr
#
[ "$okamp" -gt 0 -a "$oksnr" -gt 0 ]

#
# eof
#
