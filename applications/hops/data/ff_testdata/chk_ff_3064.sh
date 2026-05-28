#!/bin/bash
#
# $Id: chk_ff_3064.sh 4601 2026-05-27 21:47:55Z gbc $
#
# canonical test suite for fourfit3
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

[ -d $DATADIR/3064 ] || {
    echo missing data $DATADIR/3064 -- skipping test ; exit 77 ;
}

for BB in S X
do
    $verb && echo \
    rm -f ff-3064-NV-$BB.ps
    rm -f ff-3064-NV-$BB.ps
    $verb && echo \
    $fourfit -t -b NV:$BB -d diskfile:ff-3064-NV-$BB.ps \\ && echo \
        $DATADIR/3064/113-0256/4C39_25.qxajks
    $fourfit -t -b NV:$BB -d diskfile:ff-3064-NV-$BB.ps \
        $DATADIR/3064/113-0256/4C39_25.qxajks 2>/dev/null 1>&2
    [ -f "./ff-3064-NV-$BB.ps" ] ||
        { echo ./ff-3064-NV-$BB.ps missing && exit 2 ; }

    # pluck out lines containing the snr and amp, parse and check

    line=$(grep '7570 9384' ./ff-3064-NV-$BB.ps)
    IFS='()'
    read a amp$BB b <<<"$line"
    echo $BB amp line is $line

    lsnr=$(grep '7570 9653' ./ff-3064-NV-$BB.ps)
    IFS='()'
    read a snr$BB b <<<"$lsnr"
    echo $BB snr line is $lsnr

done

# ff-3064-NV-S.ps: 7570 9384 M (22.613) SR
amplowS=22.3
amphighS=22.9
# ff-3064-NV-X.ps: 7570 9384 M (56.980) SR
amplowX=56.5
amphighX=57.5
# ff-3064-NV-S.ps: 7570 9653 M (154.0) SR
snrlowS=153.0
snrhighS=155.0
# ff-3064-NV-X.ps: 7570 9653 M (500.6) SR
snrlowX=500.0
snrhighX=501.0

okampS=$(echo "$ampS>$amplowS && $ampS<$amphighS" | bc)
$verb &&
  echo okampS is $okampS and "$amplowS < $ampS < $amphighS" is expected
oksnrS=$(echo "$snrS>$snrlowS && $snrS<$snrhighS" | bc)
$verb &&
  echo oksnrS is $oksnrS and "$snrlowS < $snrS < $snrhighS" is expected

okampX=$(echo "$ampX>$amplowX && $ampX<$amphighX" | bc)
$verb &&
  echo okampX is $okampX and "$amplowX < $ampX < $amphighX" is expected
oksnrX=$(echo "$snrX>$snrlowX && $snrX<$snrhighX" | bc)
$verb &&
  echo oksnrX is $oksnrX and "$snrlowX < $snrX < $snrhighX" is expected

[ "$okampS" -gt 0 -a "$oksnrS" -gt 0 ] || exit 1;
[ "$okampX" -gt 0 -a "$oksnrX" -gt 0 ] || exit 2;

exit 0
#
# eof
#
