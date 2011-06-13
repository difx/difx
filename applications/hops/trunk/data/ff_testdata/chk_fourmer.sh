#!/bin/sh
#
# $Id: chk_fourmer.sh 313 2011-06-07 14:09:32Z gbc $
#
# a test for fourmer
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
cwd=$DATADIR
$verb && echo DATADIR=$DATADIR && echo cwd=$cwd

[ -z "$GS_DEVICE" ] && GS_DEVICE=nullpage
$verb && echo GS_DEVICE=$GS_DEVICE

export HOPS_FOURMER_FG=E
export HOPS_FOURMER_JOIN=HL

# sources
rdir=3365
hidir='094-0644_HIGH'
lodir='094-0644_LOW'
hi='3365/094-0644_HIGH/3C273.vmudbk'
lo='3365/094-0644_LOW/3C273.vlncsf'

# output
wdir='3365/094-0644_'$HOPS_FOURMER_JOIN

# clean slate
rm -rf $wdir $rdir/$hidir $rdir/$lodir
mkdir -p $wdir

[ -d $rdir ] || mkdir -p $rdir
[ -d $rdir/$hidir ] || mkdir -p $rdir/$hidir
[ -d $rdir/$lodir ] || mkdir -p $rdir/$lodir
[ -d $srcdir/testdata/$rdir ] || { echo Missing $rdir data ; exit 2; }
[ -d $srcdir/testdata/$rdir/$hidir ] || { echo Missing $hidir data ; exit 2; }
[ -d $srcdir/testdata/$rdir/$lodir ] || { echo Missing $lodir data ; exit 2; }

$verb && echo \
cp $srcdir/testdata/$rdir/$hidir/* $rdir/$hidir
cp $srcdir/testdata/$rdir/$hidir/* $rdir/$hidir
$verb && echo \
cp $srcdir/testdata/$rdir/$lodir/* $rdir/$lodir
cp $srcdir/testdata/$rdir/$lodir/* $rdir/$lodir

### fourmer works best locally
cd $wdir
fourmer $DATADIR/$lo $DATADIR/$hi 1>fourmer-out.out 2>&1

lines=`cat fourmer-out.out | wc -l`
[ "$lines" -eq 5 ] || { echo lines is $lines, not 5 ; exit 1 ; }

set -- `ls 3C273*`
[ $# -gt 1 ] && { echo too many root files ; exit 2 ; }
root=$1
time=`echo $root | sed s/3C273.//`

set -- `ls *.$time | wc -l` 0
[ $1 -eq 7 ] || { echo wrong number of fourmed files ; exit 3; }

### back to top level
cd $cwd

freqlist='a b c d e f g h i j k l m n o  q r s t u v w x y z A B C D E'

for bl in SP SO OP
do
    rm -f $bl-{ff,fx}.out
    $verb && echo \
    fourfit -m 1 -b $bl -t $wdir/$root \\ && echo \
	set freqs $freqlist pc_mode manual
    fourfit -m 1 -b $bl -t $wdir/$root \
	set freqs $freqlist pc_mode manual >$bl-ff.out 2>&1
    pcp=`grep pc_phases $bl-ff.out`
    [ -n "$pcp" ] || { echo no pc_phases in $bl-ff.out; continue ; }
    $verb && echo \
    fourfit -m 1 -b $bl $wdir/$root \\ && echo \
	set freqs $freqlist pc_mode manual $pcp
    fourfit -m 1 -b $bl $wdir/$root \
	set freqs $freqlist pc_mode manual $pcp >$bl-fx.out 2>&1
done

set -- `ls ??-f?.out | wc -l` 0
[ $1 -eq 6 ] || { echo fourfit of formed files problem ; exit 4; }

set -- `ls -s 3365/094-0644_HL/*.E.[123].$time` 0 .1. 0 .2. 0 .3.
[ $1 -ge 100 ] || echo file $2 is too small
[ $3 -ge 100 ] || echo file $4 is too small
[ $5 -ge 100 ] || echo file $6 is too small
[ $1 -ge 100 -a $3 -gt 100 -a $5 -gt 100 ]

#
# eof
#
