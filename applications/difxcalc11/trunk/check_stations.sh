#!/bin/bash
#
# verify that hard-coded limits are consistent
#
#[gbc@gefera trunk]$ grep Max_Stat src/cmxst11.i | grep =
#      Parameter(Max_Stat = 41)
#[gbc@gefera trunk]$ grep MaxStat src/d_input.i | grep =
#      Parameter (MaxStat = 41)
#[gbc@gefera trunk]$ grep Nstation2 src/c2poly.i | grep =
#      Parameter (Nstation2 = 41)
# srcdir and verbose may be supplied via environment variables
#
[ -n "$srcdir" ] || srcdir=.
[ -n "$verbose" ] && verbose=true || verbose=false
set -- `grep Max_Stat $srcdir/src/cmxst11.i |& grep = | tr -s ')(=' '   '` \
    `grep MaxStat $srcdir/src/d_input.i |& grep = | tr -s ')(=' '   '` \
    `grep Nstation2 $srcdir/src/c2poly.i |& grep = | tr -s ')(=' '   '`
[ $# -eq 9 ] || {
    echo Sources not found or have changed too much, dollar-hash is $#
    exit 1
}
a=$3 b=$6 c=$9
$verbose &&
    echo grep gave $# items &&
    echo Max_Stat is $a &&
    echo MaxStat is $b &&
    echo Nstation2 is $c
b=$((b - 1))
err=0
[ $a -eq $b ] || { echo Max_Stat is $a and MaxStat-1 is $b ; err=$(($err+1)); }
[ $c -eq $b ] || { echo Nstation2 is $c and MaxStat-1 is $b ; err=$(($err+1)); }
$verbose && echo Exit status $err
exit $err
#
## eof
#
