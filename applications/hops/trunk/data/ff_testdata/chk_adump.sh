#!/bin/sh
#
# $Id: chk_adump.sh 330 2011-06-10 13:32:10Z gbc $
#
# canonical test suite for adump
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`

#rdir="$DATADIR/2843/321-1701_0552+398"
#targ="0552+398"
time=oifhak

rm -f alist.out
$verb && echo \
alist ??.?.?.$time 2>/dev/null 1>&2
alist ??.?.?.$time 2>/dev/null 1>&2

$verb && echo \
adump -i alist.out -o adump.out snr phase
adump -i alist.out -o adump.out snr phase

lines=`cat adump.out | wc -l`
$verb && echo lines is $lines

[ "$lines" -eq 10 ]

#
# eof
#
