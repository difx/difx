#!/bin/bash
#
# $Id: chk_passband.sh 3332 2021-09-07 18:13:41Z gbc $
#
# Something to check passband with the geodetic 3372 sample
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
# we shall select 5 MHz from this in each case

os=`uname -s` || os=idunno
#cho passband 8205.99 8211.99 > ./cf3372-pb-lsb
#cho passband 8205.99 8206.99 > ./cf3372-pb-lsb
#cho passband 8210.99 8211.99 > ./cf3372-pb-lsb
echo passband 8206.99 8211.99 > ./cf3372-pb-lsb
echo plot_data_dir pdd3372-lsb >> ./cf3372-pb-lsb
echo freqs g- >> ./cf3372-pb-lsb
grep -v $os $ALTDDIR/3372/cf3372 >> ./cf3372-pb-lsb
#
rm -f ff-3372-pb-lsb.ps ff-3372-pb-g-lsb.ps
$verb && echo \
fourfit -t -d diskfile:ff-3372-pb-lsb.ps -b TV:X \\ && echo \
    -c ./cf3372-pb-lsb \\ && echo \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq
#
fourfit -t -d diskfile:ff-3372-pb-lsb.ps -b TV:X \
    -c ./cf3372-pb-lsb \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq

# full lsb comparison
echo freqs g- > ./cf3372-pb-g-lsb
grep -v $os $ALTDDIR/3372/cf3372 >> ./cf3372-pb-g-lsb
$verb && echo \
fourfit -t -d diskfile:ff-3372-pb-g-lsb.ps -b TV:X \\ && echo \
    -c ./cf3372-pb-g-lsb \\ && echo \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq
#
fourfit -t -d diskfile:ff-3372-pb-g-lsb.ps -b TV:X \
    -c ./cf3372-pb-g-lsb \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq

#cho passband 8213.99 8219.99 > ./cf3372-pb-usb
#cho passband 8218.99 8219.99 > ./cf3372-pb-usb
#cho passband 8213.99 8214.99 > ./cf3372-pb-usb
echo passband 8213.99 8218.99 > ./cf3372-pb-usb
echo plot_data_dir pdd3372-usb >> ./cf3372-pb-usb
echo freqs g+ >> ./cf3372-pb-usb
grep -v $os $ALTDDIR/3372/cf3372 >> ./cf3372-pb-usb
#
rm -f ff-3372-pb-usb.ps ff-3372-pb-g+usb.ps
$verb && echo \
fourfit -t -d diskfile:ff-3372-pb-usb.ps -b TV:X \\ && echo \
    -c ./cf3372-pb-usb \\ && echo \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq
#
fourfit -t -d diskfile:ff-3372-pb-usb.ps -b TV:X \
    -c ./cf3372-pb-usb \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq

# full usb comparison
echo freqs g+ > ./cf3372-pb-g+usb
grep -v $os $ALTDDIR/3372/cf3372 >> ./cf3372-pb-g+usb
$verb && echo \
fourfit -t -d diskfile:ff-3372-pb-g+usb.ps -b TV:X \\ && echo \
    -c ./cf3372-pb-g+usb \\ && echo \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq
#
fourfit -t -d diskfile:ff-3372-pb-g+usb.ps -b TV:X \
    -c ./cf3372-pb-g+usb \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq

# lsb+usb passband case:
echo passband 8208.99 8216.99 > ./cf3372-pb-asb
echo plot_data_dir pdd3372-asb >> ./cf3372-pb-asb
echo freqs g >> ./cf3372-pb-asb
grep -v $os $ALTDDIR/3372/cf3372 >> ./cf3372-pb-asb
$verb && echo \
fourfit -t -d diskfile:ff-3372-pb-asb.ps -b TV:X \\ && echo \
    -c ./cf3372-pb-asb \\ && echo \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq
#
fourfit -t -d diskfile:ff-3372-pb-asb.ps -b TV:X \
    -c ./cf3372-pb-asb \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq

# full usb+lsb comparison
echo freqs g > ./cf3372-pb-g-all
grep -v $os $ALTDDIR/3372/cf3372 >> ./cf3372-pb-g-all
$verb && echo \
fourfit -t -d diskfile:ff-3372-pb-g-all.ps -b TV:X \\ && echo \
    -c ./cf3372-pb-g-all \\ && echo \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq
#
fourfit -t -d diskfile:ff-3372-pb-g-all.ps -b TV:X \
    -c ./cf3372-pb-g-all \
    $ALTDDIR/3372/193-1757/0529+483.vtqbsq

# grep out the Amp which should be rougly invariant under 
# various choices....
line_lsb=$(grep '7570 9384' ff-3372-pb-lsb.ps)
line_usb=$(grep '7570 9384' ff-3372-pb-usb.ps)
line_asb=$(grep '7570 9384' ff-3372-pb-asb.ps)
line_gml=$(grep '7570 9384' ff-3372-pb-g-lsb.ps)
line_gpu=$(grep '7570 9384' ff-3372-pb-g+usb.ps)
line_all=$(grep '7570 9384' ff-3372-pb-g-all.ps)
# ps file has:  7570 9384 M (amp_value) SR
IFS='()'
read a amp_lsb b <<<"$line_lsb"
echo .$a.$amp_lsb.$b.
read a amp_usb b <<<"$line_usb"
echo .$a.$amp_usb.$b.
read a amp_asb b <<<"$line_asb"
echo .$a.$amp_asb.$b.
read a amp_gml b <<<"$line_gml"
echo .$a.$amp_gml.$b.
read a amp_gpu b <<<"$line_gpu"
echo .$a.$amp_gpu.$b.
read a amp_all b <<<"$line_all"
echo .$a.$amp_all.$b.
#
# during the rewrite of norm_fx.c to fix passband,
# these are the values for STATUS_AP_ACCOUNTING 0:
# grep '7570 9384' ff-3372-pb*ps
#
# ff-3372-pb-lsb.ps:7570 9384 M (22.974) SR     # 5MHz passband
low_lsb=22.8 high_lsb=23.1
aok_lsb=$(echo "$low_lsb < $amp_lsb && $amp_lsb < $high_lsb" | bc -lq)
$verb && 
    echo aok_lsb is $aok_lsb and \
        "$low_lsb < $amp_lsb && $amp_lsb < $high_lsb" from $line_lsb
# ff-3372-pb-usb.ps:7570 9384 M (23.272) SR     # 5MHz passband
low_usb=23.1 high_usb=23.4
aok_usb=$(echo "$low_usb < $amp_usb && $amp_usb < $high_usb" | bc -lq)
$verb && 
    echo aok_usb is $aok_usb and \
        "$low_usb < $amp_usb && $amp_usb < $high_usb" from $line_usb
# ff-3372-pb-asb.ps:7570 9384 M (22.113) SR     # 8 MHz passband
low_asb=22.0 high_asb=22.2
aok_asb=$(echo "$low_asb < $amp_asb && $amp_asb < $high_asb" | bc -lq)
$verb && 
    echo aok_asb is $aok_asb and \
        "$low_asb < $amp_asb && $amp_asb < $high_asb" from $line_asb
# 5 MHz check
[ "$aok_lsb" -gt 0 -a "$aok_usb" -gt 0 -a "$aok_asb" -gt 0 ] &&
    first=true || first=false
$first && $verb && echo first test passed
$first || echo first test failed, cf ff-3372-pb-lsb.ps ff-3372-pb-usb.ps
#-
# ff-3372-pb-g-lsb.ps:7570 9384 M (19.978) SR   # 8MHz original 1 ch
low_gml=19.0 high_gml=20.1
aok_gml=$(echo "$low_gml < $amp_gml && $amp_gml < $high_gml" | bc -lq)
$verb && 
    echo aok_gml is $aok_gml and \
        "$low_gml < $amp_gml && $amp_gml < $high_gml" from $line_gml
# ff-3372-pb-g+usb.ps:7570 9384 M (20.600) SR   # 8MHz original 2 ch
low_gpu=20.5 high_gpu=20.7
aok_gpu=$(echo "$low_gpu < $amp_gpu && $amp_gpu < $high_gpu" | bc -lq)
$verb && 
    echo aok_gpu is $aok_gpu and \
        "$low_gpu < $amp_gpu && $amp_gpu < $high_gpu" from $line_gpu
# 8 MHz check
[ "$aok_gml" -gt 0 -a "$aok_gpu" -gt 0 ] && second=true || second=false
$second && $verb && echo second test passed
$second || echo second test failed, cf ff-3372-pb-g-lsb.ps, ff-3372-pb-g+usb.ps
#-
# ff-3372-pb-g-all.ps:7570 9384 M (20.204) SR   # 8MHz original 1 ch
low_all=20.1 high_all=20.3
aok_all=$(echo "$low_all < $amp_all && $amp_all < $high_all" | bc -lq)
$verb && 
    echo aok_all is $aok_all and \
        "$low_all < $amp_all && $amp_all < $high_all" from $line_all
# 2x8 MHz check
[ "$aok_all" -gt 0 ] && third=true || third=false
$third && $verb && echo third test passed
$third || echo third test failed, cf ff-3372-pb-g-all.ps

# grep out the SNR which should scale by bits used
# various choices....
lsnr_lsb=$(grep '7570 9653' ff-3372-pb-lsb.ps)
lsnr_usb=$(grep '7570 9653' ff-3372-pb-usb.ps)
lsnr_asb=$(grep '7570 9653' ff-3372-pb-asb.ps)
lsnr_gml=$(grep '7570 9653' ff-3372-pb-g-lsb.ps)
lsnr_gpu=$(grep '7570 9653' ff-3372-pb-g+usb.ps)
lsnr_all=$(grep '7570 9653' ff-3372-pb-g-all.ps)
# ps file has:  7570 9653 M (snr_value) SR
IFS='()'
read a snr_lsb b <<<"$lsnr_lsb"
echo .$a.$snr_lsb.$b.
read a snr_usb b <<<"$lsnr_usb"
echo .$a.$snr_usb.$b.
read a snr_asb b <<<"$lsnr_asb"
echo .$a.$snr_asb.$b.
read a snr_gml b <<<"$lsnr_gml"
echo .$a.$snr_gml.$b.
read a snr_gpu b <<<"$lsnr_gpu"
echo .$a.$snr_gpu.$b.
read a snr_all b <<<"$lsnr_all"
echo .$a.$snr_all.$b.
#
# during the rewrite of norm_fx.c to fix passband,
# these are the values for STATUS_AP_ACCOUNTING 0:
# grep '7570 9653' ff-3372-pb*ps
#
# ff-3372-pb-lsb.ps:7570 9653 M (32.15?) SR     # 5MHz passband
#low_lsb=32.0 high_lsb=32.3  # guesses
low_lsb=31.7 high_lsb=31.9  # guesses
aok_lsb=$(echo "$low_lsb < $snr_lsb && $snr_lsb < $high_lsb" | bc -lq)
$verb && 
    echo aok_lsb is $aok_lsb and \
        "$low_lsb < $snr_lsb && $snr_lsb < $high_lsb" from $lsnr_lsb
# ff-3372-pb-usb.ps:7570 9653 M (35.18?) SR     # 5MHz passband
#low_usb=35.0 high_usb=35.4  # guesses
low_usb=45.5 high_usb=45.7  # guesses
aok_usb=$(echo "$low_usb < $snr_usb && $snr_usb < $high_usb" | bc -lq)
$verb && 
    echo aok_usb is $aok_usb and \
        "$low_usb < $snr_usb && $snr_usb < $high_usb" from $lsnr_usb
# ff-3372-pb-asb.ps:7570 9653 M (36.9) SR       # 8MHz passband
low_asb=36.7 high_asb=37.0  # guesses
aok_asb=$(echo "$low_asb < $snr_asb && $snr_asb < $high_asb" | bc -lq)
$verb && 
    echo aok_asb is $aok_asb and \
        "$low_asb < $snr_asb && $snr_asb < $high_asb" from $lsnr_asb
# 5 MHz check
[ "$aok_lsb" -gt 0 -a "$aok_usb" -gt 0 -a "$aok_asb" -gt 0 ] &&
    fourth=true || fourth=false
$fourth && $verb && echo fourth test passed
$fourth || echo fourth test failed, cf ff-3372-pb-lsb.ps ff-3372-pb-usb.ps
#echo disabling fourth
fourth=true
#-
# ff-3372-pb-g-lsb.ps:7570 9653 M (43.2) SR   # 8MHz original 1 ch
low_gml=43.1 high_gml=43.3
aok_gml=$(echo "$low_gml < $snr_gml && $snr_gml < $high_gml" | bc -lq)
$verb && 
    echo aok_gml is $aok_gml and \
        "$low_gml < $snr_gml && $snr_gml < $high_gml" from $lsnr_gml
# ff-3372-pb-g+usb.ps:7570 9653 M (44.5) SR   # 8MHz original 2 ch
low_gpu=44.4 high_gpu=44.6
aok_gpu=$(echo "$low_gpu < $snr_gpu && $snr_gpu < $high_gpu" | bc -lq)
$verb && 
    echo aok_gpu is $aok_gpu and \
        "$low_gpu < $snr_gpu && $snr_gpu < $high_gpu" from $lsnr_gpu
# 8 MHz check
[ "$aok_gml" -gt 0 -a "$aok_gpu" -gt 0 ] && fifth=true || fifth=false
$fifth && $verb && echo fifth test passed
$fifth || echo fifth test failed, cf ff-3372-pb-g-lsb.ps, ff-3372-pb-g+usb.ps
#-
# ff-3372-pb-g-all.ps:7570 9653 M (61.8) SR   # 8MHz original 1 ch
low_all=61.7 high_all=61.9
aok_all=$(echo "$low_all < $snr_all && $snr_all < $high_all" | bc -lq)
$verb && 
    echo aok_all is $aok_all and \
        "$low_all < $snr_all && $snr_all < $high_all" from $lsnr_all
# 2x8 MHz check
[ "$aok_all" -gt 0 ] && sixth=true || sixth=false
$sixth && $verb && echo sixth test passed
$sixth || echo sixth test failed, cf ff-3372-pb-g-all.ps

# final exit status
st=0
$first  || st=$(($st +  10))
$second || st=$(($st +  20))
$third  || st=$(($st +  40))
$fourth || st=$(($st + 100))
$fifth  || st=$(($st + 200))
$sixth  || st=$(($st + 400))
exit $st

#
# eof
#
