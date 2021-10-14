#!/bin/bash
#
# $Id: chk_notches.sh 3333 2021-09-07 20:30:41Z gbc $
#
# Something to check notches using the some ALMA test data
# (The original version of this test used Mk4/hdw data and
# thus used norm_xf() not norm_fx() which is where develop
# is not proceeding.
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
export ALTDDIR=`cd $srcdir/testdata; pwd`
cwd=$DATADIR
$verb && echo DATADIR=$DATADIR && echo cwd=$cwd

wdir='3727/026-1123'
[ -d $ALTDDIR/$wdir ] || { echo No 3727 data; exit 1; }
root=$ALTDDIR/$wdir/J1337-1257.12SVG0
[ -n "$root" -a -s "$root" ] || { echo no root ; exit 1; }

cf=cf3727.notch
pdd=pdd3727
#echo 'freqs a b c d e f g h i j k l m n o p q r s t u v w x y z A B C D E F' > $cf
echo "plot_data_dir $pdd" > $cf
echo 'pc_mode manual' >> $cf
# a 212162.796875 .. + 58
# b 212221.390625 .. + 58
# ..
# E 213920.609375 .. + 58
# F 213979.203125 .. + 58
echo -n 'notches ' >> $cf
# 4x 58/4 MHz notches per channel, lined up to be obvious
# effectively reducing the bandwidth by 2, we expect the
# amplitude to be unchanged and the SNR to drop by ~sqrt(2).
# 671.7 / sqrt(2) * 1.013 =  481.1, i.e. agreement to 1.3%
fr=212166.000000
to=212173.250000
true && for n in {1..32}
do
  frx=$fr tox=$to
  for n in {1..4}
  do
    echo -n " $frx $tox" >> $cf
    frx=`echo $frx + 14.500000 | bc`
    tox=`echo $tox + 14.500000 | bc`
  done
  fr=`echo $fr + 58.593750 | bc`
  to=`echo $to + 58.593750 | bc`
done
echo '' >> $cf
grep -v notches $cf > $cf.not
mv $cf $cf.got

export HOPS_PLOT_DATA_MASK=0x87FFFFFF
rm -rf $pdd
$verb && echo \
    fourfit -t -c $cf.not -d diskfile:ff-3727-DA.not.ps -b DA -P LL $root
    fourfit -t -c $cf.not -d diskfile:ff-3727-DA.not.ps -b DA -P LL $root
for o in `ls $pdd/*` ; do mv $o $o.not ; done
$verb && echo \
    fourfit -t -c $cf.got -d diskfile:ff-3727-DA.got.ps -b DA -P LL $root
    fourfit -t -c $cf.got -d diskfile:ff-3727-DA.got.ps -b DA -P LL $root
for o in `ls $pdd/* | grep -a -v not` ; do mv $o $o.got ; done

[ -s ff-3727-DA.not.ps -a -s ff-3727-DA.got.ps ] && second=true || second=false
$second && $verb && echo second test passed
$second || echo second test failed, cf ff-3727-DA.not.ps ff-3727-DA.got.ps

# grep out the amplitude which should be roughly invariant
line_not=$(grep '7570 9384' ff-3727-DA.not.ps)
line_got=$(grep '7570 9384' ff-3727-DA.got.ps)
# ps file has:  7570 9384 M (amp_value) SR
IFS='()'
read a amp_not b <<<"$line_not"
echo .$a.$amp_not.$b.
read a amp_got b <<<"$line_got"
echo .$a.$amp_got.$b.
# ff-3727-DA.not.ps:7570 9384 M (37.295) SR
low_not=37.195 high_not=37.395
aok_not=$(echo "$low_not < $amp_not && $amp_not < $high_not" | bc -lq)
$verb && 
    echo aok_not is $aok_not and \
        "$low_not < $amp_not && $amp_not < $high_not" from $line_not
# ff-3727-DA.got.ps:7570 9384 M (37.152) SR
low_got=37.052 high_got=37.252
aok_got=$(echo "$low_got < $amp_got && $amp_got < $high_got" | bc -lq)
$verb && 
    echo aok_got is $aok_got and \
        "$low_got < $amp_got && $amp_got < $high_got" from $line_got
[ "$aok_not" -gt 0 -a "$aok_got" -gt 0 ] && third=true || third=false
$third && $verb && echo third test passed
$third || echo third test failed, cf ff-3727-DA.not.ps ff-3727-DA.got.ps

# grep out the snr which should scale by bits used
lsnr_not=$(grep '7570 9653' ff-3727-DA.not.ps)
lsnr_got=$(grep '7570 9653' ff-3727-DA.got.ps)
# ps file has:  7570 9384 M (snr_value) SR
IFS='()'
read a snr_not b <<<"$lsnr_not"
echo .$a.$snr_not.$b.
read a snr_got b <<<"$lsnr_got"
echo .$a.$snr_got.$b.
# ff-3727-DA.not.ps:7570 9653 M (671.7) SR
low_not=671.0 high_not=672.4
aok_not=$(echo "$low_not < $snr_not && $snr_not < $high_not" | bc -lq)
$verb && 
    echo aok_not is $aok_not and \
        "$low_not < $snr_not && $snr_not < $high_not" from $lsnr_not
# ff-3727-DA.got.ps:7570 9653 M (669.1) SR
#low_got=668.6 high_got=669.9
low_got=481.1 high_got=481.3
aok_got=$(echo "$low_got < $snr_got && $snr_got < $high_got" | bc -lq)
$verb && 
    echo aok_got is $aok_got and \
        "$low_got < $snr_got && $snr_got < $high_got" from $lsnr_got
[ "$aok_not" -gt 0 -a "$aok_got" -gt 0 ] && fourth=true || fourth=false
$fourth && $verb && echo fourth test passed
$fourth || echo fourth test failed, cf ff-3727-DA.not.ps ff-3727-DA.got.ps

# final exit status
st=0
$second || st=$(($st + 10))
$third  || st=$(($st + 20))
$fourth || st=$(($st + 40))
exit $st

#
# eof
#
