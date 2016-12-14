#!/bin/sh

# Usage: $0 expr [rate [true|false [otheroptions]]]
# e.g.      vObo [125000  [true]]
# expr is something that matches the file(s) you are interested in.
# rate is 125000 packets per second unless specified
# true|false is whether to reuse previous cache
# other options may be specified as a quoted list; e.g. '-xhier=4 ...'

USAGE="$0 expression [rate [true|false [true|false [vdifuseoptions]]]]

This script populates mount points for one scan in \$home
(which if not supplied in the environment defaults to \$HOME/difx/data)
and it assumes a pair of module subgroups (12 and 34) with data
to be found in /mnt/disks/\$s/?/\$data, where \$s is taken from
an environment variable \$sm (defaults to '12 34') and \$data
defaults to 'data'.  If you've copied the data elsewhere set
\$mount as a replacement for '/mnt/disks'.

Here the expression is some character sequence (RE) that matches
the scan(s) of interest.  The packet rate is assumed 125000 pkts/sec
unless specified in the second argument.  The 3rd argument (true|false)
controls whether to re-use an existing cache (mod-??-expression).  A
fourth argument (true|false) specifies whether a tracelog should be
retained.  (The default is false, which deletes the tracefiles written
to /tmp after vdifuse exits.)

Any additional arguments are passed directly to vdifuse, to do this
you will need to specify the initial 4 arguments fully.  Use

    vdifuse --help

for more information about that.  To recap on the environment variables:
    home    where to work (\$HOME/difx/data)
    sm      the list of sub-module groups ('12 34')
    data    an alternate scatter-gather dir name ('data')
    mount   alternate mount points ('/mnt/disks')

A filelist for use with vex2difx is generated in <something>.flist.
"

[ $# -lt 1 -o "$1" = '--help' ] && { echo "$USAGE" ; exit 0 ; }

[ -n "$home" ] || home=$HOME/difx/data
[ -d "$home" ] || mkdir -p $home
cd $home

[ -z "$sm"   ] && sm='12 34'
[ -z "$data" ] && data='data'
[ -z "$mount" ] && mount='/mnt/disks'
expr=$1
rate=${2-'125000'}
save=${3-'true'}
trace=${4-'false'}
# this prevents agressive, out-of-order kernel-readaheads
#opts='-o sync_read -o allow_other'
# this is more agressive, and we think it might work, now.
opts='-o async_read -o allow_other'
[ $# -gt 4 ] && shift 4 && opts="$opts $@"

$trace && trop='-vt' || trop='-t'

for m in $sm
do
    fusermount -u ./mnt$m
    $save || rm -f mod-$m-$expr
    [ -d ./mnt$m ] || mkdir ./mnt$m
    [ -f mod-$m-$expr ] && {
        echo reusing mod-$m-$expr
        echo \
        vdifuse $trop -u mod-$m-$expr $opts ./mnt$m
        vdifuse $trop -u mod-$m-$expr $opts ./mnt$m
        # make filelist
        [ -f mod-$m-$expr.flist ] || {
            echo \
            vdifuse -m mod-$m-$expr -xlist=$home/mnt$m \> mod-$m-$expr.flist
            vdifuse -m mod-$m-$expr -xlist=$home/mnt$m  > mod-$m-$expr.flist
        }
        true
    } || {
        echo creating mod-$m-$expr
        echo vdifuse $trop -a mod-$m-$expr -xm6sg -xrate=$rate \\ && echo \
            -xinclpatt=$expr $opts ./mnt$m $mount/[$m]/?/$data
        eval vdifuse $trop -a mod-$m-$expr -xm6sg -xrate=$rate \
            -xinclpatt=$expr $opts ./mnt$m $mount/[$m]/?/$data
        # make filelist
        echo \
        vdifuse -m mod-$m-$expr -xlist=$home/mnt$m \> mod-$m-$expr.flist
        vdifuse -m mod-$m-$expr -xlist=$home/mnt$m  > mod-$m-$expr.flist
    }
    ls -l mod-$m-$expr mod-$m-$expr.flist
done

# this should always work
ls -lh ./mnt??/se*/*/??/*.vdif
ls -l mod-*-$expr.flist

# eof
