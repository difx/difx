#!/bin/sh
#
# $Id: chk_avefix.sh 1763 2017-06-02 18:08:08Z gbc $
#
# test the 'average' fix
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
$verb && echo DATADIR=$DATADIR

# captive data set, version 5
atd=$srcdir/testdata/average
many=$atd/080-low-2-subset.1sec
aold=$atd/080-low-2-subset.averaged
cmp1=./averaged-old-1.out
cmp2=./averaged-old-2.out
cmp3=./averaged-old-3.out
cmp4=./averaged-old-4.out

# first verify backwards compatibility
grep -v average $aold > $cmp1
fdg=-0.95
$verb && echo \
average -s $fdg $many \| grep -v written.with \| grep -v average \| cmp - $cmp1
average -s $fdg $many | grep -v written.with | grep -v average | cmp - $cmp1
[ $? -eq 0 ] || {
    echo backward compatibility test failed
    echo \
    average -s $fdg $many \| grep -v written.with \| \
        grep -v average \| diff - $cmp1
    average -s $fdg $many | grep -v written.with | \
        grep -v average | diff - $cmp1
    exit 1;
}

# next verify the incorrect fix
grep -v average $aold | tr -s ' ' \\012 > $cmp2
fdg=0.95
$verb && echo \
average -s $fdg $many \| grep -v written.with \| grep -v average \| \
    tr -s ' ' \\\\012 \| cmp - $cmp2
average -s $fdg $many | grep -v written.with | grep -v average | \
    tr -s ' ' \\012 | cmp - $cmp2 > /dev/null
[ $? -eq 1 ] || {
    echo backward compatibility test failed
    echo \
    average -s $fdg $many \| grep -v written.with \| \
        grep -v average \| tr -s \' \' \\012 \| diff - $cmp2
    average -s $fdg $many | grep -v written.with | \
        grep -v average | tr -s ' ' \\012 | diff - $cmp2
    exit 1;
}

average -s $fdg $many | grep -v written.with |\
   grep -v average | tr -s ' ' \\012 | diff - $cmp2 > $cmp3
average         $many | grep -v written.with |\
   grep -v average | tr -s ' ' \\012 | diff - $cmp2 > $cmp4
set -- `cat $cmp3`
$verb && echo fudged: $*
newfdgsnr=$3
oldsnr1=$6
set -- `cat $cmp4`
$verb && echo newsnr: $*
newval=$3
newsnr=$5
oldval=$8
shift 6
oldsnr2=$4

$verb && set -x
[ "$oldsnr1" = "$oldsnr2" -a \
  "$newval" = 1.44 -a "$oldval" = 1.43 -a \
  "$oldsnr1" = '9.323' -a "$newsnr" = '20.34' ]

#
# EOF
#
