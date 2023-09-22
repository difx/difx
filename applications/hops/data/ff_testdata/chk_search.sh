#!/bin/sh
#
# $Id: chk_search.sh 408 2011-08-18 15:08:59Z gbc $
#
# Something to check search
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`
cwd=$DATADIR
$verb && echo DATADIR=$DATADIR && echo cwd=$cwd

rm -f search.ps search.out

# there are many -d options:
# -d all gets us the huge range ...
fringex -i 20 -d 27x27 -r alist-aedit-X0X.out |\
average -o search.avg > fas.out 2>&1
HOPS_SEARCH_REMLIMIT=0.6 \
search -d search.ps/cps -o search.out search.avg >> fas.out 2>&1

set -- `ls -s search.ps 2>&-` 0 0
size=$1
$verb && echo size is $size

set -- `wc -l search.out` 0 0
lines=$1
$verb && echo lines is $lines

[ -f search.ps -a "$size" -ge 76 -a -f search.out -a "$lines" -eq 3 ]

#
# eof
#
