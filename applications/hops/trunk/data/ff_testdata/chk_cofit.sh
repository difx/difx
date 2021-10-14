#!/bin/sh
#
# $Id: chk_cofit.sh 3326 2021-09-04 13:05:05Z gbc $
#
# exercises cofit
#

verb=false
[ -n "$testverb" ] && verb=true

[ -d "$srcdir" ] || { echo srcdir not set; exit 1; }
${HOPS_SETUP-'false'} || . $srcdir/chk_env.sh
export DATADIR=`pwd`

rdir="2843/321-1701_0552+398"
targ="0552+398"
time=oifhak

[ -d $DATADIR/$rdir ] || { echo Missing 2843 data; exit 2;}

$verb && echo \
cofit -d alist-aedit.ps/ps \\ && echo \
'   -o alist-aedit.cofit alist-aedit.coavg' \\ && echo \
'   1>alist-aedit-1.cofit 2>alist-aedit-2.cofit'

cofit -d alist-aedit.ps/ps \
    -o alist-aedit.cofit alist-aedit.coavg \
    1>alist-aedit-1.cofit 2>alist-aedit-2.cofit

set -- `wc -l alist-aedit.cofit alist-aedit-1.cofit alist-aedit-2.cofit`
$verb && echo wc is $@

[ "$1" -eq 6 -a "$3" -eq 59 -a "$5" -eq 5 ]

#
# eof
#
