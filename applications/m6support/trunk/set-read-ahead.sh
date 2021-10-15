#!/bin/sh
# The default appears to be 128, but 4096 might be better.
#
default=128
faster=4096
[ $# -eq 1 ] || { echo "Usage: $0 default|faster|kb" ; exit 0; }
case $1 in
default)    kb=128  ;;
faster)     kb=4096 ;;
*)          kb=$1   ;;
esac

root=`df / | grep -v Filesystem | cut -d' ' -f1 | tr -d 0-9 | sed s+/dev/sd++`
skip=/sys/block/sd$root/queue/read_ahead_kb
echo skipping root partition $skip

[ `id -u` -eq 0 ] || { echo you must be root for this; exit 1; }

for rakb in /sys/block/sd?/queue/read_ahead_kb
do
    [ "$rakb" = "$skip" ] && { echo skipping $rakb ; continue ; }
    echo $kb > $rakb
    cat $rakb
done

#
# eof
#
