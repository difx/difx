#!/bin/bash
#
# Stupid script to report on filled fraction by
# dplane since we seem to be doing this alot.
#
bpp=${1-'8224'}
sg0=${2-'12'}
sg1=${3-'34'}

set -- `grep filled /var/log/mark6/dplane-daemon.log |\
        awk '{s[$2] += $4}END{\
            printf "stream 0 filled " s[0] " or %f bytes\n", s[0] * '$bpp';\
            printf "stream 1 filled " s[1] " or %f bytes\n", s[1] * '$bpp';}'`
s0fb=$6
echo $1 $2 $3 $4 $5 $s0fb $7 ; shift 7
s1fb=$6
echo $1 $2 $3 $4 $5 $s1fb $7 ; shift 7

s0kb=`df | grep -v meta | grep mnt.disks/'['$sg0']' |\
      awk '{sum+=$3*1.0}END{printf "%.0f", sum }'`
s1kb=`df | grep -v meta | grep mnt.disks/'['$sg1']' |\
      awk '{sum+=$3*1.0}END{printf "%.0f", sum }'`
echo stream 0 used $s0kb kB
echo stream 1 used $s1kb kB

s0ff=`echo $s0fb / $s0kb / 1024 | bc -lq`
s1ff=`echo $s1fb / $s1kb / 1024 | bc -lq`

echo stream 0 fill rate $s0ff
echo stream 1 fill rate $s1ff

#
# eof
#
