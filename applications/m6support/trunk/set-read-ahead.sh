#!/bin/sh
#
# (c) Massachusetts Institute of Technology, 2013..2023
# (c) Geoffrey B. Crew, 2013..2023
#
# The default appears to be 128, but 4096 might be better.
#
# This utility may be used as root to tune the kernel read-ahead
# setting.  The 4096 here means that the kernel will assume you
# will want at least the next 4MB every time you start access.
#
default=128
faster=4096
match='mnt/disks/./.'
[ $# -eq 1 ] || {
    echo "Usage: $0 default|faster|#kb [match]"
    echo
    echo "  default   is 128 kb read-ahead"
    echo "  faster    is 4096 kb read-ahead"
    echo "  match     is $match"
    echo
    echo "This script manipulates the kernel's read-ahead"
    echo "policy and it can only be run by root."
    exit 0;
}
[ `id -u` -eq 0 ] || { echo you must be root for this; exit 1; }

case $1 in
default)    kb=128  ;;
faster)     kb=4096 ;;
*)          kb=$1   ;;
esac
[ $# -eq 2 ] && match="$2"

devs=`df -h | grep dev | grep "$match" |\
      awk '{print $1}' | cut -d/ -f3 | tr -d '[0-9]' | sort`

for dev in $devs
do
    rakb=/sys/block/$dev/queue/read_ahead_kb
    [ -f "$rakb" ] || { echo skipping $rakb; continue; }
    echo -n $rakb `cat $rakb` " -> "
    echo $kb > $rakb
    cat $rakb
done

#
# eof
#
