#!/bin/bash
#
# $Id: chk_cohfit.sh 4572 2026-05-21 20:47:03Z gbc $
#
# exercises cohfit
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

ls -l alist-aedit.coavg
che=`type -p cohfit`
[ -x "$che" ] || { echo No cohfit executable ; exit 77; }
echo cohfit is $che
viable=`cohfit < /dev/null 2>&1 | grep Wrote`
[ "$viable" = "cohfit: Wrote 0 coherence-analyzed output records" ] || {
    echo This is a you-lose cohfit, so we punt ; exit 77; }

$verb && echo \
cohfit -d alist-aedit.ps/ps \\ && echo \
'   -o alist-aedit.cohfit alist-aedit.coavg' \\ && echo \
'   1>alist-aedit-1.cohfit 2>alist-aedit-2.cohfit'

cohfit -d alist-aedit.ps/ps \
    -o alist-aedit.cohfit alist-aedit.coavg \
    1>alist-aedit-1.cohfit 2>alist-aedit-2.cohfit
echo return status $?

set -- `wc -l alist-aedit.cohfit alist-aedit-1.cohfit alist-aedit-2.cohfit`
$verb && echo wc is $@

echo "$1" == 6 and "$3" == 0 and "$5" gt 70 and "$5" lt 115
[ "$1" -eq 6 -a "$3" -eq 0 -a "$5" -gt 70 -a "$5" -lt 115 ] || {
    echo output file sizes are not within expected range
    exit 3
}
echo file sizes are within normal limits

# acid test
ampfits=`grep -E 'Wrote|redchi' alist-aedit-2.cohfit |\
         sed 's/stat.*iters=[0-9]*//'`
$verb && echo ampfits is this: && echo "$ampfits"
set -- `echo "$ampfits" | cksum`
$verb && echo checksum of that is ${1-'0'} of ${2-'0'} lines

# so finally
echo test that $1 == 2742801947 and $2 == 858

[ $1 -eq 2742801947 ] || { echo big number fail ; exit 4; }
[ $1 -eq 2742801947 ] && { echo big number ok; }
[ $2 -eq 858 ] || { echo little number fail ; exit 5; }
[ $2 -eq 858 ] && { echo little number ok; }
exit 0
#
# eof
#
