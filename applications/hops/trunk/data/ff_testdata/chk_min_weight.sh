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
grep -v $os $DATADIR/3571/cf3571_244-1249 > ./cf3571-ok
sed '/mbd_anchor sbd/a\
min_weight 0.999
' < ./cf3571-ok > ./cf3571-mw

$verb && type fourfit
$verb && printenv > ff-3571-env.out

rm -f ff-3571-ok.ps
#run fourfit normally
$verb && echo \
fourfit -pt -d diskfile:ff-3571-ok.ps -b GE -c ./cf3571-ok -PI \\ && echo \
    $DATADIR/3571/244-1717/0727-115.zbgwce
fourfit -pt -d diskfile:ff-3571-ok.ps -b GE -c ./cf3571-ok -PI \
    $DATADIR/3571/244-1717/0727-115.zbgwce
[ -f ./ff-3571-ok.ps ] || { echo ./ff-3571-ok.ps missing && exit 2 ; }

#run fourfit with min weight
rm -f ff-3571-mw.ps
$verb && echo \
fourfit -pt -d diskfile:ff-3571-mw.ps -b GE -c ./cf3571-mw -PI \\ && echo \
    $DATADIR/3571/244-1717/0727-115.zbgwce
#run fourfit
fourfit -pt -d diskfile:ff-3571-mw.ps -b GE -c ./cf3571-mw -PI \
    $DATADIR/3571/244-1717/0727-115.zbgwce
[ -f ./ff-3571-mw.ps ] || { echo ./ff-3571-mw.ps missing && exit 2 ; }

IFS='()'
# pluck out line containing the snr and parse it
lineok=$(grep '7570 9653' ./ff-3571-ok.ps)
read a snrok b <<<"$lineok"
lowok=400.4 highok=400.6 snrtok=$(echo "$snrok>$lowok && $snrok<$highok" | bc)
$verb && echo snrtok is $snrtok and "$lowok<$snrok<$highok" from: $lineok

# pluck out line containing the integration time and parse it
lineok=$(grep '7570 9519' ./ff-3571-ok.ps)
read a intok b <<<"$lineok"
lowok=29.990 highok=30.000 inttok=$(echo "$intok>$lowok && $intok<$highok" | bc)
$verb && echo inttok is $inttok and "$lowok<$intok<$highok" from: $lineok

# pluck out line containing the amplitude and parse it
lineok=$(grep '7570 9384' ./ff-3571-ok.ps)
read a ampok b <<<"$lineok"
lowok=13.369 highok=13.371 amptok=$(echo "$ampok>$lowok && $ampok<$highok" | bc)
$verb && echo amptok is $amptok and "$lowok<$ampok<$highok" from: $lineok

# pluck out line containing the snr and parse it
linemw=$(grep '7570 9653' ./ff-3571-mw.ps)
read a snrmw b <<<"$linemw"
lowmw=393.6 highmw=393.8 snrtmw=$(echo "$snrmw>$lowmw && $snrmw<$highmw" | bc)
$verb && echo snrtmw is $snrtmw and "$lowmw<$snrmw<$highmw" from: $linemw

# pluck out line containing the integration time and parse it
linemw=$(grep '7570 9519' ./ff-3571-mw.ps)
read a intmw b <<<"$linemw"
lowmw=28.990 highmw=29.010 inttmw=$(echo "$intmw>$lowmw && $intmw<$highmw" | bc)
$verb && echo inttmw is $inttmw and "$lowmw<$intmw<$highmw" from: $linemw

# pluck out line containing the amplitude and parse it
linemw=$(grep '7570 9384' ./ff-3571-mw.ps)
read a ampmw b <<<"$linemw"
lowmw=13.366 highmw=13.370 amptmw=$(echo "$ampmw>$lowmw && $ampmw<$highmw" | bc)
$verb && echo amptmw is $amptmw and "$lowmw<$ampmw<$highmw" from: $linemw

[ "$snrtok" -eq 1 ]  || { echo snr-ok fail ; exit 1 ; }
[ "$inttok" -eq 1 ]  || { echo int-ok fail ; exit 2 ; }
[ "$amptok" -eq 1 ]  || { echo amp-ok fail ; exit 3 ; }
[ "$snrtmw" -eq 1 ]  || { echo snr-mw fail ; exit 4 ; }
[ "$inttmw" -eq 1 ]  || { echo int-mw fail ; exit 5 ; }
[ "$amptmw" -eq 1 ]  || { echo amp-mw fail ; exit 6 ; }

exit 0
#
# eof
#
