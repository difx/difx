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
#
srcdir=.
set -- `grep Max_Stat $srcdir/src/cmxst11.i | grep = | tr -s ')(=' '   '` \
    `grep MaxStat $srcdir/src/d_input.i | grep = | tr -s ')(=' '   '` \
    `grep Nstation2 $srcdir/src/c2poly.i | grep = | tr -s ')(=' '   '`
a=$3 b=$6 c=$9
b=$((b - 1))
err=0
[ $a -eq $b ] || { echo Max_Stat is $a and MaxStat-1 is $b ; err=$(($err+1)); }
[ $c -eq $b ] || { echo Nstation2 is $c and MaxStat-1 is $b ; err=$(($err+1)); }
#echo $a
#echo $b
#echo $c
exit $err
#
## eof
#
