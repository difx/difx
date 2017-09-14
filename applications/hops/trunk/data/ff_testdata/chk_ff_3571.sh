#!/bin/bash
#
# $Id: chk_ff_3571.sh Thu Nov 17 10:19:27 EST 2016 jpb
#
# canonical test suite for fourfit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

os=`uname -s` || os=idunno
grep -v $os $DATADIR/3571/cf3571_244-1249 > ./cf3571

$verb && type fourfit
$verb && printenv > ff-3571-env.out

rm -f ff-3571.ps
$verb && echo fourfit -pt -b GE -c cf3571_244-1249 -PI ./244-171/0727-115.zbgwce

#run fourfit
fourfit -pt -d diskfile:ff-3571.ps -b GE -c ./cf3571 -PI $DATADIR/3571/244-1717/0727-115.zbgwce
[ -f ./ff-3571.ps ] || { echo ./ff-3571.ps missing && exit 2 ; }

# pluck out line containing the snr and parse it
line=$(grep '7570 9653' ./ff-3571.ps)

IFS='()'
read a snr b <<<"$line"

# snr bounds
low=395.9
high=401
#high=396.3 (this value is from before the change to IXY fourfit amplitudes)
aok=$(echo "$snr>$low && $snr<$high" | bc)
$verb && echo aok is $aok and "$low < $snr < $high" is expected from: $line

[ "$aok" -gt 0 ]

#
# eof
#
