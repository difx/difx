#!/bin/bash
#
# $Id: chk_misc_3372.sh 4383 2025-07-20 21:38:08Z gbc $
#
# canonical test suite for fourfit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`cd $srcdir/testdata; pwd`

cat > ./cf3372.misc <<EOF
* four new features
  noautofringes true
  mod4numbering true
  polfringnames true
  fringeout_dir `pwd`/fringes3772
  mbdrplopt 1 3 5
  sb_win -0.3 0.3
*
* back to ff-3372 conf file...
*
EOF

os=`uname -s` || os=idunno
grep -v $os $DATADIR/3372/cf3372 >> ./cf3372.misc

rm -f ff-misc-3372-*.ps
$verb && echo \
$fourfit -d diskfile:ff-misc-3372-%02d.ps -b \\?\\?:X \\ && echo \
    -c ./cf3372.misc \\ && echo \
    $DATADIR/3372/193-1757/0529+483.vtqbsq

$fourfit -d diskfile:ff-misc-3372-%02d.ps -b \?\?:X \
    -c ./cf3372.misc \
    $DATADIR/3372/193-1757/0529+483.vtqbsq # 2>/dev/null 1>&2

# pluck out lines containing the snr and amp, parse and check

# numbers are for TV:X
NN=00
[ -f ./ff-misc-3372-$NN.ps ] || { echo ./ff-misc-3372-$NN.ps missing && exit 2 ; }

# ff-3372.ps:7570 9384 M (21.078) SR
line=$(grep '7570 9384' ./ff-misc-3372-$NN.ps)
IFS='()'
read a amp b <<<"$line"
low=21.058
high=21.088
okamp=$(echo "$amp>$low && $amp<$high" | bc)
$verb && echo okamp is $okamp and "$low < $amp < $high" is expected from: $line
# ff-2836.ps:7570 9653 M (144.1) SR
lsnr=$(grep '7570 9653' ./ff-misc-3372-$NN.ps)
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
