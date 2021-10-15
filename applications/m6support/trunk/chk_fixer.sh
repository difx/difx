#!/bin/bash
#
# Script to check scan_fixer
#
[ -n "$testverb" ] && v=-v || v=''
[ -n "$testverb" ] && verb=true || verb=false

[ -z "$testverb" ] && testverb=0
while [ "$testverb" -gt 1 ]
do v=${v}v ; testverb=$(($testverb - 1)) ; done

[ -x ./push_test ] || {
    echo ./push_test is missing, forced skip
    exit 77
}
df=data/to-fix.vdif
sf=./scan_fixer

# an ALMA scan with 1000 packets, frame wrapping in the middle
$verb && echo \
./push_test -H $df -n1000 -pedge=1 -pvtp=0 -r0 file
./push_test -H $df -n1000 -pedge=1 -pvtp=0 -r0 file
[ -s $df ] || {
    echo $df is missing, fail
    exit 77
}

# test one, just make a copy
$verb && echo \
$sf -o data/fixed-1.vdif $v $df
$sf -o data/fixed-1.vdif $v $df

# test two, drop down in the middle, continue
$verb && echo \
$sf -o data/fixed-2.vdif -j 4016000 $v $df
$sf -o data/fixed-2.vdif -j 4016000 $v $df

# test three, mangle the sample and fix it
# use 100, skip 100, use 100, skip 100 ...
nn=0
files=''
for ii in {0..4}
do
    $verb && echo \
    dd if=$df of=data/runt-$ii.vdif bs=8032 count=100 skip=$nn
    dd if=$df of=data/runt-$ii.vdif bs=8032 count=100 skip=$nn
    nn=$(($nn + 200))
    files="$files data/runt-$ii.vdif"
done
$verb && echo \
cat $files > data/tomfix.vdif
cat $files > data/tomfix.vdif
rm -f $files

# run test three
$verb && echo \
$sf -o data/fixed-3.vdif $v data/tomfix.vdif
$sf -o data/fixed-3.vdif $v data/tomfix.vdif

# test four, really mangle the sample and fix it
# similar to 3 but use non-integral buffer size.
nn=0
files=''
for ii in {0..32}
do
    $verb && echo \
    dd if=$df of=data/yuck-$ii.vdif bs=2000 count=100 skip=$nn
    dd if=$df of=data/yuck-$ii.vdif bs=2000 count=100 skip=$nn
    nn=$(($nn + 125))
    files="$files data/yuck-$ii.vdif"
done
$verb && echo \
cat $files > data/torfix.vdif
cat $files > data/torfix.vdif
rm -f $files

# run test four
$verb && echo \
$sf -o data/fixed-3.vdif $v data/torfix.vdif
$sf -o data/fixed-3.vdif $v data/torfix.vdif

#
# eof
#
