#!/bin/bash
#
# $Id: chk_ff_2843.sh 3327 2021-09-04 13:47:06Z gbc $
#
# canonical test suite for fourfit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

rm -f ff-2843.ps
$verb && echo \
fourfit -t -d diskfile:ff-2843.ps -b AI:S \\ && echo \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \\ && echo \
    set start -3

# AIT
fourfit -t -d diskfile:ff-2843.ps -b AI:S \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak \
    set start -3 2>/dev/null 1>&2
[ -f ./ff-2843.ps ] || { echo ./ff-2843.ps missing && exit 2 ; }

# pluck out lines containing the snr and amp, parse and check

# ff-2843.ps:7570 9384 M (21.664) SR
line=$(grep '7570 9384' ./ff-2843.ps)
IFS='()'
read a amp b <<<"$line"
low=21.660
high=21.668
okamp=$(echo "$amp>$low && $amp<$high" | bc)
$verb && echo okamp is $okamp and "$low < $amp < $high" is expected from: $line
# ff-2843.ps:7570 9653 M (48.2) SR
lsnr=$(grep '7570 9653' ./ff-2843.ps)
IFS='()'
read a snr b <<<"$lsnr"
low=48.1
high=48.3
oksnr=$(echo "$snr>$low && $snr<$high" | bc)
$verb && echo oksnr is $oksnr and "$low < $snr < $high" is expected from: $lsnr
#
[ "$okamp" -gt 0 -a "$oksnr" -gt 0 ]

#
# eof
#
