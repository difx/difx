#!/bin/bash
#
# $Id: chk_notches_xf.sh 3333 2021-09-07 20:30:41Z gbc $
#
# Something to check notches using the fourmered data 3365 sample
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
export ALTDDIR=`cd $srcdir/testdata; pwd`
cwd=$DATADIR
$verb && echo DATADIR=$DATADIR && echo cwd=$cwd

wdir='3365/094-0644_HL'
[ -d $wdir ] || { echo No 3365 data -- run chk_fourmer.sh first; exit 1; }
fout=OP-fx.out
[ -s $fout ] || { echo no file $fout -- run chk_fourmer.sh first; exit 1; }
root=`ls 3365/094-0644_HL/3C273.*`
[ -n "$root" -a -s "$root" ] || { echo no HL root ; exit 1; }

cf=cf3365.notch
# p is doubled
echo 'freqs a b c d e f g h i j k l m n o q r s t u v w x y z A B C D E' > $cf
#cho 'freqs a b c d e f g h i j k l n n o ' > $cf
#cho 'freqs q r s t u v w x y z A B C D E'  > $cf
echo 'plot_data_dir pdd3365' >> $cf
echo 'pc_mode manual' >> $cf
grep pc_phases $fout >> $cf
# a 228881.000113 .. 228849.000113
# b 228913.000113 .. 228881.000113
# c 228945.000113 .. 228913.000113
# ...
# p doubled
# ...
# C 229745.000113 .. 229777.000113
# D 229777.000113 .. 229809.000113
# E 229809.000113 .. 229841.000113
# 4 6-MHz notches in each 32-MHz channel leaves 4x2 MHz/ch
# which is 1/4 of the signal which should be 1/2 the SNR.
# this data is less clear-cut, but 71 is larger that 116/2
echo -n 'notches ' >> $cf
fr=228853.0001
to=228859.0001
true && echo -n "$fr $to" >> $cf
true && for n in {1..60}    # 15ch x 4 notch lsb
do
    fr=`echo $fr + 8 | bc`
    to=`echo $to + 8 | bc`
    echo -n " $fr $to" >> $cf
done
fr=229832.0001
to=229838.0001
echo -n '       ' >> $cf
true && echo -n "$fr $to" >> $cf
true && for n in {1..60}    # 15ch x 4 notch usb
do
    fr=`echo $fr - 8 | bc`
    to=`echo $to - 8 | bc`
    echo -n " $fr $to" >> $cf
done
echo '' >> $cf
grep -v notches $cf > $cf.not
mv $cf $cf.got

export HOPS_PLOT_DATA_MASK=0x87FFFFFF
rm -rf pdd3365
$verb && echo \
    fourfit -t -c $cf.not -d diskfile:ff-3365-OP.not.ps -b OP -P LL $root
    fourfit -t -c $cf.not -d diskfile:ff-3365-OP.not.ps -b OP -P LL $root
for o in `ls pdd3365/*` ; do mv $o $o.not ; done
$verb && echo \
    fourfit -t -c $cf.got -d diskfile:ff-3365-OP.got.ps -b OP -P LL $root
    fourfit -t -c $cf.got -d diskfile:ff-3365-OP.got.ps -b OP -P LL $root
for o in `ls pdd3365/* | grep -a -v not` ; do mv $o $o.got ; done

[ -s ff-3365-OP.not.ps -a -s ff-3365-OP.got.ps ] && second=true || second=false
$second && $verb && echo second test passed
$second || echo second test failed, cf ff-3365-OP.not.ps ff-3365-OP.got.ps

# check amplitudes
line_not=$(grep '7570 9384' ff-3365-OP.not.ps)
line_got=$(grep '7570 9384' ff-3365-OP.got.ps)
#
IFS='()'
read a amp_not b <<<"$line_not"
echo .$a.$amp_not.$b.
read a amp_got b <<<"$line_got"
echo .$a.$amp_got.$b.
# ff-3365-OP.not.ps:7570 9384 M (2.419) SR
low_not=2.409 high_not=2.429
aok_not=$(echo "$low_not < $amp_not && $amp_not < $high_not" | bc -lq)
$verb && 
    echo aok_not is $aok_not and \
        "$low_not < $amp_not && $amp_not < $high_not" from $line_not
# ff-3365-OP.got.ps:7570 9384 M (2.510) SR
low_got=2.500 high_got=2.520
aok_got=$(echo "$low_got < $amp_got && $amp_got < $high_got" | bc -lq)
$verb && 
    echo aok_got is $aok_got and \
        "$low_got < $amp_got && $amp_got < $high_got" from $line_got
[ "$aok_not" -gt 0 -a "$aok_got" -gt 0 ] && third=true || third=false
$third && $verb && echo third test passed
$third || echo third test failed, cf ff-3365-OP.not.ps ff-3365-OP.got.ps

# check amplitudes
lsnr_not=$(grep '7570 9653' ff-3365-OP.not.ps)
lsnr_got=$(grep '7570 9653' ff-3365-OP.got.ps)
#
IFS='()'
read a amp_not b <<<"$lsnr_not"
echo .$a.$amp_not.$b.
read a amp_got b <<<"$lsnr_got"
echo .$a.$amp_got.$b.
# ff-3365-OP.not.ps:7570 9653 M (111.9) SR
low_not=111.0 high_not=112.8
aok_not=$(echo "$low_not < $amp_not && $amp_not < $high_not" | bc -lq)
$verb && 
    echo aok_not is $aok_not and \
        "$low_not < $amp_not && $amp_not < $high_not" from $lsnr_not
# ff-3365-OP.got.ps:7570 9653 M (116.1) SR
#low_got=115.1 high_got=117.1
low_got=71.1 high_got=71.3
aok_got=$(echo "$low_got < $amp_got && $amp_got < $high_got" | bc -lq)
$verb && 
    echo aok_got is $aok_got and \
        "$low_got < $amp_got && $amp_got < $high_got" from $lsnr_got
[ "$aok_not" -gt 0 -a "$aok_got" -gt 0 ] && fourth=true || fourth=false
$fourth && $verb && echo fourth test passed
$fourth || echo fourth test failed, cf ff-3365-OP.not.ps ff-3365-OP.got.ps

# final exit status
st=0
$second || st=$(($st + 10))
$third  || st=$(($st + 20))
$fourth || st=$(($st + 40))
exit $st

#
# eof
#
