#!/bin/sh
#
# $Id: chk_alist.sh 330 2011-06-10 13:32:10Z gbc $
#
# canonical test suite for alist
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`

#rdir="$DATADIR/2843/321-1701_0552+398"
#targ="0552+398"
time=oifhak

files=`ls -1 ??.?.?.$time | wc -l`
[ "$files" -gt 0 ] || { echo missing files--run chk_baselines.sh; exit 2; }

rm -f alist.out

$verb && echo \
alist ??.?.?.$time 2>/dev/null 1>&2
alist ??.?.?.$time 2>/dev/null 1>&2

lines=`cat alist.out | wc -l`
$verb && echo lines is $lines

[ "$lines" -eq 9 ]

#
# eof
#
