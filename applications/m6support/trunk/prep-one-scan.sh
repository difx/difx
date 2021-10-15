#!/bin/sh

# Usage: $0 expr [rate [true|false [otheroptions]]]
# e.g.      vObo [125000  [true]]
# expr is something that matches the file(s) you are interested in.
# rate is 125000 packets per second unless specified
# true|false is whether to reuse previous cache
# other options may be specified as a quoted list; e.g. '-xhier=4 ...'

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
    mount   alternate mount points ('/mnt/disks')
    flist   replacement for the filelist suffix
    noema   true or false for 4 interlaced sub-module groups
            (sm='1 2 3 4') and -xm6noema
    debug   turns on debugging if = v or vv or vvv or ... vvvvvvv
            and forces the tracelog option to true

mod-\$m-\$expression.filelist is a filelist for use in vex2difx.

For example:
    prep-one-scan.sh e21a14_Aa_No003

makes all No003? scans available from on a Mark6 with ALMA modules.

If you use 'wipe' in place of the rate, everything is unmounted, and
any mod-\$m-\$expression caches are removed.  Trace logs appear in
/tmp/vdifuse-trace.<processid> and are deleted automatically if debug
is not set (vdifuse without -v).  Periodically you may wish to remove
them with:  rm -f /tmp/vdifuse*
"

[ $# -lt 1 -o "$1" = '--help' ] && { echo "$USAGE" ; exit 0 ; }

[ -n "$home" ] || home=$HOME/difx/data
[ -d "$home" ] || mkdir -p $home
cd $home

[ -z "$data" ] && data='data'
[ -z "$mount" ] && mount='/mnt/disks'
[ -z "$flist" ] && flist='filelist'
[ -z "$noema" ] && noema=false
[ -n "$vdops" ] || vdops=''

$noema && { m6sg=m6noema; sm='1 2 3 4'; } || { m6sg=m6sg; }
[ -z "$sm"   ] && sm='12 34'

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
[ -z "$debug" ] && $trace && trop='-t' || trop=''
[ "$debug" = v -o \
  "$debug" = vv -o \
  "$debug" = vvv -o \
  "$debug" = vvvv -o \
  "$debug" = vvvvv -o \
  "$debug" = vvvvvv -o \
  "$debug" = vvvvvvv ] && trop="-$debug -t" || {
    echo too much verbosity requested $debug, max is vvvvvvv, exit 1; }

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
    ls -l mod-$m-$expr mod-$m-$expr.$flist
done

# this should always work
ls -lh ./mnt*/se*/*/??/*.vdif
ls -l mod-*-$expr.$flist

# eof
