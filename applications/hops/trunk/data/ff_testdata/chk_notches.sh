#!/bin/bash
#
# $Id: chk_notches.sh 2399 2018-06-19 19:54:09Z gbc $
#
# Something to check notches using the fourmered data sample
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
export ALTDDIR=`cd $srcdir/testdata; pwd`
cwd=$DATADIR
$verb && echo DATADIR=$DATADIR && echo cwd=$cwd

# ref freq chan g is 8212.99 MHz, bw is 8 MHz

os=`uname -s` || os=idunno
#cho passband 8205.99 8211.99 > ./cf3372-pb-lsb
#cho passband 8205.99 8206.99 > ./cf3372-pb-lsb
#cho passband 8210.99 8211.99 > ./cf3372-pb-lsb
echo passband 8206.99 8211.99 > ./cf3372-pb-lsb
echo plot_data_dir pdd3372-lsb >> ./cf3372-pb-lsb
echo freqs g- >> ./cf3372-pb-lsb
grep -v $os $ALTDDIR/3372/cf3372 >> ./cf3372-pb-lsb
#
rm -f ff-3372-pb-lsb.ps
$verb && echo \
fourfit -t -d diskfile:ff-3372-pb-lsb.ps -b TV:X \\ && echo \
    -c ./cf3372-pb-lsb \\ && echo \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq
#
fourfit -t -d diskfile:ff-3372-pb-lsb.ps -b TV:X \
    -c ./cf3372-pb-lsb \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq

rm -f ff-3372-pb-usb.ps
#cho passband 8213.99 8219.99 > ./cf3372-pb-usb
#cho passband 8218.99 8219.99 > ./cf3372-pb-usb
#cho passband 8213.99 8214.99 > ./cf3372-pb-usb
echo passband 8213.99 8218.99 > ./cf3372-pb-usb
echo plot_data_dir pdd3372-usb >> ./cf3372-pb-usb
echo freqs g+ >> ./cf3372-pb-usb
grep -v $os $ALTDDIR/3372/cf3372 >> ./cf3372-pb-usb
#
rm -f ff-3372-pb-usb.ps
$verb && echo \
fourfit -t -d diskfile:ff-3372-pb-usb.ps -b TV:X \\ && echo \
    -c ./cf3372-pb-usb \\ && echo \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq
#
fourfit -t -d diskfile:ff-3372-pb-usb.ps -b TV:X \
    -c ./cf3372-pb-usb \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq


line_lsb=$(grep '7570 9384' ff-3372-pb-lsb.ps)
line_usb=$(grep '7570 9384' ff-3372-pb-usb.ps)
#
IFS='()'
read a amp_lsb b <<<"$line_lsb"
echo .$a.$amp_lsb.$b.
read a amp_usb b <<<"$line_usb"
echo .$a.$amp_usb.$b.
#
low_lsb=22.8 high_lsb=23.1
aok_lsb=$(echo "$low_lsb < $amp_lsb && $amp_lsb < $high_lsb" | bc -lq)
$verb && 
    echo aok_lsb is $aok_lsb and \
        "$low_lsb < $amp_lsb && $amp_lsb < $high_lsb" from $line_lsb
#
low_usb=23.1 high_usb=23.4
aok_usb=$(echo "$low_usb < $amp_usb && $amp_usb < $high_usb" | bc -lq)
$verb && 
    echo aok_usb is $aok_usb and \
        "$low_usb < $amp_usb && $amp_usb < $high_usb" from $line_usb
[ "$aok_lsb" -gt 0 -a "$aok_usb" -gt 0 ]  || exit 1

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
echo -n 'notches ' >> $cf
fr=228853.0001
to=228859.0001
true && echo -n "$fr $to" >> $cf
true && for n in {1..60}
do
    fr=`echo $fr + 8 | bc`
    to=`echo $to + 8 | bc`
    echo -n " $fr $to" >> $cf
done
fr=229832.0001
to=229838.0001
echo -n '       ' >> $cf
true && echo -n "$fr $to" >> $cf
true && for n in {1..60}
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

[ -s ff-3365-OP.not.ps -a -s ff-3365-OP.got.ps ] || exit 2

line_not=$(grep '7570 9384' ff-3365-OP.not.ps)
line_got=$(grep '7570 9384' ff-3365-OP.got.ps)
#
IFS='()'
read a amp_not b <<<"$line_not"
echo .$a.$amp_not.$b.
read a amp_got b <<<"$line_got"
echo .$a.$amp_got.$b.
#
low_not=2.40 high_not=2.44
aok_not=$(echo "$low_not < $amp_not && $amp_not < $high_not" | bc -lq)
$verb && 
    echo aok_not is $aok_not and \
        "$low_not < $amp_not && $amp_not < $high_not" from $line_not
#
low_got=2.50 high_got=2.52
aok_got=$(echo "$low_got < $amp_got && $amp_got < $high_got" | bc -lq)
$verb && 
    echo aok_got is $aok_got and \
        "$low_got < $amp_got && $amp_got < $high_got" from $line_got
[ "$aok_not" -gt 0 -a "$aok_got" -gt 0 ]  || exit 3
#
# eof
#
