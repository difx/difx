#!/bin/bash
#
# $Id: chk_ff_2611.sh 4601 2026-05-27 21:47:55Z gbc $
#
# canonical test suite for fourfit3
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

[ -d $DATADIR/2611 ] || {
    echo missing data $DATADIR/2611 -- skipping test ; exit 77 ;
}

rm -f ff-2611.ps
$verb && echo \
$fourfit -t -d diskfile:ff-2611.ps \\ && echo \
    $DATADIR/2611/062-094600/3C345.nrxoiv \\ && echo \
    set start -34

$fourfit -t -d diskfile:ff-2611.ps \
    $DATADIR/2611/062-094600/3C345.nrxoiv \
    set start -34 2>/dev/null 1>&2
[ -f ./ff-2611.ps ] || { echo ./ff-2611.ps missing && exit 2 ; }

# pluck out lines containing the snr and amp, parse and check

# ff-2611.ps: 7570 9384 M (873.115) SR
line=$(grep '7570 9384' ./ff-2611.ps)
IFS='()'
read a amp b <<<"$line"
low=873.0
high=874.0
okamp=$(echo "$amp>$low && $amp<$high" | bc)
$verb && echo okamp is $okamp and "$low < $amp < $high" is expected from: $line
# ff-2611.ps: 7570 9653 M (441.5) SR
lsnr=$(grep '7570 9653' ./ff-2611.ps)
IFS='()'
read a snr b <<<"$lsnr"
low=441.0
high=442.0
oksnr=$(echo "$snr>$low && $snr<$high" | bc)
$verb && echo oksnr is $oksnr and "$low < $snr < $high" is expected from: $lsnr

[ "$okamp" -gt 0 -a "$oksnr" -gt 0 ]
#
# eof
#
