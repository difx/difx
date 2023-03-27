#!/bin/sh
#
# (c) Massachusetts Institute of Technology, 2013..2023
# (c) Geoffrey B. Crew, 2013..2023
#
# Script designed to make one or more files readable as a flat
# vdif file using vdifuse.  The defaults are correct for a Mark6
# with ALMA data, but environment variables are good enough for all.
#
# Usage: $0 expr   [rate    [true|false [otheroptions]]]
# e.g.      e22c20 [125000  [true]]
# 
# expr is something that matches the file(s) you are interested in.
# rate is 125000 packets per second unless specified
# true|false is whether to reuse or remove previous cache
# other vdifuse options may be specified as a quoted list; e.g. '-xhier=4 ...'
# 

USAGE="$0 expression [rate [true|false [true|false [vdifuseoptions]]]]

    expression      some regex(3) match for filenames
    rate            if not 125000 packets per second
    true|false      whether to reuse a module cache (defaults true)
    true|false      whether to save the tracelog (defaults true)
    vdifuseoptions  see vdifuse --help for these

This script populates mount points for one (or more) scan(s) in \$home
(\$HOME/difx/data), assuming a pair of module subgroups (12 and 34)
with data to be found in /mnt/disks/\$s/?/\$data.  The module cache
(where vdifuse stores what is has learned) is saved as mod-??-expression.
A file list is generated as mod-\$m-\$expression.filelist referencing \$home.

Some changes are available with environment variables:
    home    where to work (\$HOME/difx/data)
    sm      the list of sub-module groups ('12 34')
    data    an alternate scatter-gather dir name ('data')
    mount   mount point ('/mnt/disks'); dir with s/d/data/files.vdif
    flist   replacement for the filelist suffix
    noema   true or false for 4 interlaced sub-module groups
            (sm='1 2 3 4') and -xm6noema
    vlba    true or false for one module (and larger blocking)
    debug   turns on debugging if = v or vv or vvv or ... vvvvvvv
            and forces the tracelog option to true

mod-\$m-\$expression.filelist is a filelist for use in vex2difx.

For example:
    prep-one-scan.sh e21a14_Aa_No003

makes all No003? scans available from on a Mark6 with ALMA modules
within \$home/mnt{12,34}/sequences, creating filelists and a re-usable
cache (named for this scan).  For more details try also vdifuse --help.

If you use 'wipe' in place of the rate, everything is unmounted, and
any mod-\$m-\$expression caches are removed.  Trace logs appear in
/tmp/vdifuse-trace.<processid> and are deleted automatically if debug
is not set (vdifuse with -t only, no -v).  Periodically you may wish
to remove them with:  rm -f /tmp/vdifuse*
"

[ $# -lt 1 -o "$1" = '--help' ] && { echo "$USAGE" ; exit 0 ; }

[ -n "$home" ] || home=$HOME/difx/data
[ -d "$home" ] || mkdir -p $home
echo \
cd $home
cd $home

[ -z "$data" ] && data='data'
[ -z "$mount" ] && mount='/mnt/disks'
[ -z "$flist" ] && flist='filelist'
[ -z "$noema" ] && noema=false
[ -z "$vlba" ] && vlba=false
[ -n "$vdops" ] || vdops=''

$noema && { m6sg=m6noema; sm='1 2 3 4'; } || { m6sg=m6sg; }
[ -z "$sm"   ] && sm='12 34'

$vlba && { m6sg='m6sg' vdops="$vdops -xblock=33553384" ;sm=1; }

expr=$1
rate=${2-'125000'}
save=${3-'true'}
trace=${4-'true'}
# this prevents agressive, out-of-order kernel-readaheads
#opts='-o sync_read -o allow_other'
# this is more agressive, and we think it might work, now.
opts='-o async_read -o allow_other '$vdops
[ $# -gt 4 ] && shift 4 && opts="$opts $@"

# vdifuse has verbosity levels up through 7
# tracing is always advisable
[ "$trace" = 'true' -o "$trace" = 'false' ] || trace=false
[ -z "$debug" ] && {
    $trace && trop='-t' || trop=''
}
[ -n "$debug" ] && {
    [ "$debug" = v -o \
      "$debug" = vv -o \
      "$debug" = vvv -o \
      "$debug" = vvvv -o \
      "$debug" = vvvvv -o \
      "$debug" = vvvvvv -o \
      "$debug" = vvvvvvv ] && trop="-$debug -t" || {
          echo too much verbosity requested $debug, max is vvvvvvv; exit 1;
    }
}

for m in $sm
do
    echo fusermount -u ./mnt$m
    fusermount -u ./mnt$m
    $save || rm -f mod-$m-$expr
    [ "$rate" = wipe ] && continue
    [ -d ./mnt$m ] || mkdir ./mnt$m
    [ -f mod-$m-$expr ] && {
        echo reusing mod-$m-$expr
        echo \
        vdifuse $trop -u mod-$m-$expr $opts ./mnt$m
        vdifuse $trop -u mod-$m-$expr $opts ./mnt$m
        # make filelist if not found on disk
        [ -f mod-$m-$expr.$flist ] || {
            echo \
            vdifuse -m mod-$m-$expr -xlist=$home/mnt$m \> mod-$m-$expr.$flist
            vdifuse -m mod-$m-$expr -xlist=$home/mnt$m  > mod-$m-$expr.$flist
        }
        true
    } || {
        echo creating mod-$m-$expr
        echo vdifuse $trop -a mod-$m-$expr -x$m6sg -xrate=$rate \\ && echo \
            -xinclpatt=$expr $opts ./mnt$m $mount/[$m]/?/$data
        eval vdifuse $trop -a mod-$m-$expr -x$m6sg -xrate=$rate \
            -xinclpatt=$expr $opts ./mnt$m $mount/[$m]/?/$data
        # make filelist
        echo \
        vdifuse -m mod-$m-$expr -xlist=$home/mnt$m \> mod-$m-$expr.$flist
        vdifuse -m mod-$m-$expr -xlist=$home/mnt$m  > mod-$m-$expr.$flist
    }
    echo module cache: `ls -l mod-$m-$expr`
done

sleep 2

# this should always work
echo background vdifuse processes:
ps x | grep 'vdifuse'
echo debugging/monitoring files:
ls -l /tmp/vdifuse-trace.*
ls -l /tmp/vdifuse-bread.*
echo assembled files:
ls -lh ./mnt*/se*/*/??/*
echo module filelist:
ls -l mod-*-$expr.$flist
mount | grep fuse | grep $home

# eof
