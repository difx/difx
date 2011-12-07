#!/bin/sh
#
# $Id: chk_frmrsrch.sh 541 2011-11-14 13:20:09Z gbc $
#
# Something to check search on fourmered data.
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
cwd=$DATADIR
$verb && echo DATADIR=$DATADIR && echo cwd=$cwd

wdir='3365/094-0644_HL'
[ -d $wdir ] || { echo No 3365 data -- run chk_fourmer.sh first; exit 1; }

rm -f frmrsrch.ps frmrsrch.out

rm -f frmrsrch.alist
alist -o frmrsrch.alist $wdir > frmral.out 2>&1
[ -f frmrsrch.alist ] || { echo no alist -- bailing ; exit 2; }

# there are many -d options:
# -d all gets us the huge range ...
fringex -i 30 -d 27x27 -r frmrsrch.alist |\
average -o frmrsrch.avg > frmras.out 2>&1
#HOPS_SEARCH_REMLIMIT=0.6 \
search -d frmrsrch.ps/cps -o frmrsrch.out frmrsrch.avg >> frmras.out 2>&1

set -- `ls -s frmrsrch.ps 2>&-` 0 0
size=$1
$verb && echo size is $size

set -- `wc -l frmrsrch.out` 0 0
lines=$1
$verb && echo lines is $lines

[ -f frmrsrch.ps -a "$size" -ge 45 -a -f frmrsrch.out -a "$lines" -eq 3 ]

#
# eof
#
