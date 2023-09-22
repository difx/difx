#!/bin/bash
#
# (c) Massachusetts Institute of Technology, 2013..2023
# (c) Geoffrey B. Crew, 2013..2023
#
# Script to hammer on the disks, aka conditioning.
#
size=10g
slots=1234
prefix=crap
suffix=zero
maxfiles=10000
cpus=0
cps=8
label=unknown
slot=0
purge=true
nominal=64.25
cullrate=0.00
idle=0.00
doit=true
hammerkill=$HOME/hammerkill
hammerpause=$HOME/hammerpause

# our tools; mentioned here to allow replacements
timer=/usr/bin/time
maker=/usr/sbin/xfs_mkfile
chker=/usr/bin/cksum
bcalc=/usr/bin/bc
fmt=%Y%m%d%H%M%S

# see the table at end of file
xanon="100g 40g 10g 1g 100m 40m 10m "
canon=`grep '^.cksum' $0 | cut -c8-11 | tr -s \\\\012 ' '`

[ x"$xanon" = x"$canon" ] || { echo scripting error ; exit 13 ; }

USAGE='Usage: '`basename $0`" [options]

where the options are any of these:

    size=$size         a canonical xfs_mkfile size
    slots=$slots       Mark6 slots with modules to condition
    prefix=$prefix      prefix to empty files written
    suffix=$suffix      suffix to empty files written
    maxfiles=$maxfiles   maximum number of files / disk
    purge=$purge       remove files when done
    cullrate=$cullrate    cull files slower than this
    idle=$idle        insert sleeps to work slower
    doit=$doit        if false, only report the plan

will write empty files to all the disks of the slots appearing
in the slots argument, up to a maximum of maxfiles per disk.
It should also stop after the disk is too full to continue.
The canonical write sizes are ones where the checksum is known
for comparison on the read stage; use one of these:
$canon

Once launched, the script spawns several jobs per slot, logging
its output to a per-slot file.  To stop or pause this script:

    touch $hammerkill
    touch $hammerpause

respectively.  The file must then be removed to continue or to
run the script on a subsequent occasion.

If purge is true, the temporary files (and anything else in the
data directories) will be removed if the script is allowed to
finish.  You can also accomplish this manually with maxfiles=0.
This will not touch files parked in other directories.

Note that if you want to condition completely, you should fill
it completely (or plan to) before setting purge=true.  You should
also completely reinitialize the modules to get a clean state if
you have time to do so.

If the cullrate is nonzero, files with write rates slower than
this value will be culled to a location that is not purged on
each disk and consequently will survive the purge.  This will
thereby reduce the module capacity, but retain the required
write speed across the remaining module capacity.

If for some reason you want the process to take longer, set the
'idle' parameter to something greater than the default, 0.00.  When
nonzero, the script will insert pauses between files.  You can
and should use 'doit=false' to see what would happen.  These calculation
use a nominal read/write rate of 'nominal=$nominal' which you can adjust
for higher fidelity if you have more information than this script.

The script expects to use these programs (which must be installed):
$timer
$maker
$chker
$bcalc
"
[ -n "$1" -a "$1" = '--help' ] && { echo "$USAGE" ; exit 0 ; }
args="$@"
while [ $# -gt 0 ] ; do eval "$1"; shift; done

# make sure the user set this correctly
[ "x$doit" = 'xtrue' -o x"$doit" = 'xfalse' ] || {
    echo "doit must be true or false, not '$doit'"
    exit 2
}

# find out how many CPUs so we can share the resources
[ "$cpus" -eq 0 ] && cpus=`grep processor /proc/cpuinfo | wc -l`

for e in $maker $chker $bcalc 
do [ -x $e ] || { echo $e is not installed ; exit 1; } ; done

dotime() {
    first=`date +%s`
    didit=`$3 $4 $5 $6`
    final=`date +%s`
    ff=`expr $final - $first`
    echo $didit $ff
}

[ -x $timer ] || {
    timer=dotime
# test:
#   $timer -f %e xfs_mkfile -v 1g /mnt/disks/1/0/data/junk
#   exit 5
}

[ -f "$hammerkill" ] && {
    echo "I see $hammerkill, you need to remove it"
    exit 2
}
[ -f "$hammerpause" ] && {
    echo "I see $hammerpause, you need to remove it"
    exit 2
}

#
# slot=0 is the parent
#
[ "$slot" -eq 0 ] && {
    s='' c=0 j=''
    [ `expr $slots : '.*1.*'` -gt 0 ] && s="$s 1" && c=$(($c + 1))
    [ `expr $slots : '.*2.*'` -gt 0 ] && s="$s 2" && c=$(($c + 1))
    [ `expr $slots : '.*3.*'` -gt 0 ] && s="$s 3" && c=$(($c + 1))
    [ `expr $slots : '.*4.*'` -gt 0 ] && s="$s 4" && c=$(($c + 1))
    [ "$cps" -eq 0 ] && cps=$(($cpus / $c))
    echo
    echo Assigning $cps CPUs per slot
    for sl in $s
    do
        [ -f /mnt/disks/.meta/$sl/0/eMSN ] &&
            lab=`cat /mnt/disks/.meta/$sl/0/eMSN | cut -c1-8 | tr -d /` ||
            lab=Module%$sl
        xrgs=${args/slots=$slots/}
        echo && echo $0 \\ && echo \
        "   $xrgs" slot=$sl label=$lab cpus=$cps \&
        $0 "$xrgs" slot=$sl label=$lab cpus=$cps &
        j="$j $!"
        sleep .5
    done

    echo
    trap "kill $j 2>/dev/null" 0 1 2 15
    echo "# waiting for $j to complete ..."
    $doit || echo " ... which should not be long ..."
    wait
    echo done && echo
}

# for cull activities
greater() {
    ( echo $1 \> $2 ) | $bcalc -lq
}
cull=false
[ `greater $cullrate 0.00` -eq 1 ] && cull=true

# for rate calculations
dorate() { # MB secs
    ( echo scale=2 ; echo $1 / 1024 / 1024 / $2 ) | $bcalc -lq
}

# for time estimation
dopause() { # pause = secs * idle * 64.25 / nominal
    ( echo scale=2 ; echo $1 \* $2 \* 64.25 / $3 ) | $bcalc -lq
}
dototal() { # hours = maxfiles pause secs nominal
    (echo scale=2;echo $1 \* \( $2 + 1.7 \* $3 \* 64.25 / $4 \) / 3600) |\
    $bcalc -lq
}

#
# slot>0 is a child
#
[ $slot -ge 1 -a $slot -le 4 ] && {
    host=`hostname`
    [ -d $HOME/logs ] && ldir=$HOME/logs || ldir=.
    log=$ldir/$host-$label-hammer-`date -u +$fmt`.log
    exec 1>$log 4>&2 2>&1

    eval set -- {1..$cpus}
    cpees="$@"

    set -- `grep '^#cksum' $0`
    sum=0 bytes=0
    while [ $# -ge 5 ]
    do [ $2 = $size ] && sum=$3 && bytes=$4 && secs=$5 ; shift 5; done
    kilos=$(($bytes / 1024))

    # convert maxfiles secs idle into pause and total
    pause=`dopause $secs $idle $nominal` || pause=0
    total=`dototal $maxfiles $pause $secs $nominal` || total='unknown'

    set -- `ls -d /mnt/disks/$slot/?`
    disks="$@"
    disk0=0 disk1=0 disk2=0 disk3=0
    disk4=0 disk5=0 disk6=0 disk7=0
    ndisks=$#

    header="$label is working on $host with $cpus assigned CPU resources
            $label to $log
            $label handling $ndisks disks with $maxfiles files / disk
            $label writing files of size $size ($bytes) w/cksum $sum
            $label cull is $cull, purge is $purge, doit is $doit
            $label secs is $secs, idle is $idle, pause is $pause
            $label nominal read/write rate is $nominal MB/s, and the
            $label total duration of the work might be $total hours."
    echo "$header" | sed -e 's/^[ ]*/# /'
    # share the log with the human
    echo "$header" | sed -e 's/^[ ]*/# /' 1>&4

    # stop here if only reporting the plan
    $doit || { rm $log ; exit 0 ; }

    # make sure data directories exist
    for disk in $disks
    do
        [ -d $disk/data ] || {
            echo $label creating data directories
            mkdir $disk/data &&
                chgrp mark6 $disk/data && chmod 2775 $disk/data &&
                ls -ld $disk/data
        }
    done

    # and also the cull directories
    $cull && for disk in $disks
    do
        [ -d $disk/cull ] || {
            echo $label creating cull directories
            mkdir $disk/cull &&
                chgrp mark6 $disk/cull && chmod 2775 $disk/cull &&
                ls -ld $disk/cull
        }
    done

    # skip this step if only purging
    [ "$maxfiles" -gt 0 ] && working=true || working=false
    while $working
    do
        jb=""
        for c in $cpees
        do
            working=false
            [ $# -lt $cpus ] && set -- $* $disks
            disk=$1 ; shift
            dnum=`basename $disk`
            eval disk$dnum=\$\(\(\$disk$dnum + 1\)\)
            eval n=\$disk$dnum
            [ "$n" -ge $maxfiles ] && continue
            free=`df $disk | awk 'NR==2{print $4}'`
            [ $free -lt $kilos ] && continue
            echo $disk \($n/$maxfiles\) on cpu-$c using $kilos of $free
            tag=`date -u +$fmt`
            file=$disk/data/$prefix-$tag.$suffix
            [ -d $disk/data ] || continue
            ( set -- `$timer -f %e $maker -v $size $file 2>&1`
              [ $# -eq 4 ] || echo MAKER timer error: "$@"
              # filename size 'bytes' elapsedsecs
              rate=`dorate $bytes $4`
              echo $@ $rate MB/s write
              set -- `$timer -f %e $chker $file 2>&1`
              [ $# -eq 4 ] || echo CHKER timer error: "$@"
              # cksum bytes filename elapsedsecs
              # rename file by write rate
              mv $3 $3.$rate
              file=$3.$rate
              [ "$1" -eq $sum ] && cs=$1-aok || cs=$1-err
              [ "$2" -eq $bytes ] && bs=$2-aok || bs=$2-err
              rate=`dorate $bytes $4`
              echo $file $cs $bs $4 $rate MB/s read
              echo sleeping for $pause ... ; sleep $pause ) &
            jb="$jb $!"
            working=true
        done
        echo "# waiting on $jb at $tag"
        trap "kill $jb 2>/dev/null" 0 1 2 15
        wait
        [ -f "$hammerkill" ] && working=false
        while [ -f "$hammerpause" ] ; do echo sleep 30 ... && sleep 30 ; done
    done

    # do not go on
    [ -f $hammerkill ] && {
        echo "exciting prematurely due to $hammerkill"
        echo "cull $cull -> false, purge $purge -> false"
        cull=false
        purge=false
    }

    # cull the slower-written files to saved location
    $cull && {
        echo culling data files at cull rate $cullrate
        for disk in $disks
        do
            data=$disk/data
            cull=$disk/cull
            for f in $data/$prefix-$tag.$suffix.*
            do
                rate=`echo $f | sed "s/.*$suffix.//"`
                comp=`greater $rate $cullrate`
                [ "$comp" -eq 0 ] && echo culling $f && mv $f $cull
            done
        done
    }

    # free the disk resources
    $purge && {
        echo "# $label purge prep"
        jr=""
        for disk in $disks
        do
            data=$disk/data
            [ -d $data-die ] && mv $data-die $data-die-$$
            mv $data $data-die && mkdir $data &&
                chgrp mark6 $data && chmod g+sw $data &&
                echo "# $label purging... $data" &&
                rm -rf $data-die* &
            jr="$jr $!"
        done

        echo "# waiting on $jr"
        trap "kill $jr 2>/dev/null" 0 1 2 15
        wait
    }
}

#
# Table of canonical checksum values and file sizes.
# secs was calculated using 64.25 MB/s (64.25 * 1024 * 1024)
#  xum name   checksum        bytes  secs
#cksum 100g 2435306624 107374182400  1600
#cksum  40g 3497339177  42949672960  640
#cksum  10g 1961958409  10737418240  160
#cksum   1g 3413741448   1073741824  16
#cksum 100m 2755649025    104857600  2
#cksum  40m 1961958409     41943040  1
#cksum  10m  248556017     10485760  1
#

exit 0
#
# eof
#
