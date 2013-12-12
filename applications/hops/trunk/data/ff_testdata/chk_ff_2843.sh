#!/bin/bash
#
# $Id: chk_ff_2843.sh 879 2013-11-07 13:24:24Z gbc $
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
fourfit -pt -b AI:S \\ && echo \
    $DATADIR/2843/321-1701_0552+398/0552+398.oifhak

# AIT
( echo sff-2843.ps; echo q ) | (
    fourfit -pt -b AI:S \
	$DATADIR/2843/321-1701_0552+398/0552+398.oifhak
) 2>/dev/null 1>&2

# pluck out line containing the snr and parse it
line=$(grep '7570 9653' ./ff-2843.ps)

IFS='()'
read a snr b <<<"$line"

# snr bounds
low=47.8
high=48.6
aok=$(echo "$snr>$low && $snr<$high" | bc)

[ $aok -gt 0 ]
#
# eof
#
