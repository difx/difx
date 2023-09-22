#!/bin/sh
#
# $Id: chk_average.sh 3326 2021-09-04 13:05:05Z gbc $
#
# this uses aedit's output, with fringex to average data
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
$verb && echo DATADIR=$DATADIR

rdir="2843/321-1701_0552+398"
targ="0552+398"
time=oifhak

umask 0002
[ -d $rdir ] || mkdir -p $rdir
[ -d $srcdir/testdata/$rdir ] || { echo Missing 2843 data; exit 2; }
[ -f alist-aedit.out ] || { echo Run chk_aedit.sh first; exit 3; }

$verb && echo cp $srcdir/testdata/$rdir/*.$time $rdir
cp $srcdir/testdata/$rdir/*.$time $rdir
chmod +w $rdir/*
files=`ls -1 *.$time 2>&1 | wc -l`
[ "$files" -gt 1 ] && mv *.$time $rdir &&
$verb && echo moved $files files to $rdir

grep -v ' S06 ' alist-aedit.out > alist-aedit-X0X.out

$verb && echo \
fringex -i all -c -r alist-aedit-X0X.out \| average -c -o alist-aedit.coavg
fringex -i all -c -r alist-aedit-X0X.out  |\
average -c -o alist-aedit.coavg 2>/dev/null 1>&2

lines=`cat alist-aedit.coavg | wc -l`
$verb && echo lines is $lines

[ "$lines" -eq 25 ]

#
# eof
#
