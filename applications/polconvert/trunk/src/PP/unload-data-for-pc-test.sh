#!/bin/bash
#
# A driver script to unload test tarballs to some new location
#
# defaults for help:

# usage message
USAGE="$0 [options]

will unpack DiFX data and/or QA2 tarball snippets created by
extract-data-for-pc-test.sh and provide necessary relocation
edits.  These options are required to do real work:

    -d --difx tarball   path to the DiFX product tarball
    -q --qa2 tarball    path to the QA2 product tarball
    -o --out path       to be populated with viable data

These modify the work done and may be optional:

    -v --verb           if set, chatter about the work (repeatable)
    -y --dry            if set, stop after gathering information
    -k --nuke           if set, delete any existing output directory
    -n --new string     new job name (parsed as expn[...] _ jobnum)
    -p --pkg string     a new qa2 package name

and these provide help:

    -h --help           this short help message
    -H --explain        an extended explanatory message
"
HELP="
This script unpacks the tarballs created by extract-data-for-pc-test.sh.
The 'wrinkle' is that DiFX file paths definitely need to be adjusted and
you can also change the job number, vex experiment name, etc. via the 
-n option.  The QA2 package is a similar untar operation, but you can
rename the tables with -p if you like.
"
# derived from /usr/share/doc/util-linux/getopt-example.bash
short='Hhvkyd:q:o:n:p:'
longy='explain,help,verb,nuke,dry,difx:,qa2:,out:,new:,pkg:'
GETOPT=$(getopt -o "$short" -l "$longy" -n $0 -- "$@")
gostat=$?
[ $gostat -ne 0 ] && { echo option parsing error $gostat; exit $gostat; }
eval set -- "$GETOPT" ; unset GETOPT
# set defaults for parsed variables
very=false verb=false user=false tale=false nuke=false dry=false
difx='difx-dir-undefined' qa2='qa2-dir-undefined' out='out-dir-undefined'
njob='same' pkg='same'
# and now parse what getopt gave us
while true; do case "$1" in
'-H'|'--explain')   tale=true;    shift; continue;;
'-h'|'--help')      user=true;    shift; continue;;
'-v'|'--verb')      $verb&&very=true;
                    verb=true;    shift; continue;;
'-y'|'--dry')       dry=true;     shift; continue;;
'-k'|'--nuke')      nuke=true;    shift; continue;;
'-d'|'--difx')      difx=$2;    shift 2; continue;;
'-q'|'--qa2')       qa2=$2;     shift 2; continue;;
'-o'|'--out')       out=$2;     shift 2; continue;;
'-n'|'--new')       njob=$2;    shift 2; continue;;
'-p'|'--pkg')       pkg=$2;     shift 2; continue;;
'--')                             shift;    break;;
*)                  echo parse error>&2;   exit 1;;
esac ; done
$user && echo "Usage: $USAGE" && exit 0
$tale && echo "$HELP" && exit 0

# settle the directory issues
[ -d "$out" ] && $nuke && rm -rf $out && $verb &&
    echo output directory $out existed and was removed
[ -d "$out" ] && { echo dir $out exists--stopping--use -k option; exit 2; }
$verb && echo "unloading tarballs '$difx' and '$qa2' with output to '$out'"

# set remapping variables that may be needed
same=false
[ -f "$difx" ] && {
    job=`tar ztf $difx | grep input | sed 's/.input$//'`
    input=$job.input
    set -- `echo $job | tr _ ' '`
    oname=$1 ; shift
    while [ $# -gt 1 ] ; do oname=${oname}_$1 ; shift ; done
    ojobn=$1 ; shift
    oexpn=`expr $job : '\([a-z0-9A-Z]*\).*'`
    ojmid=`expr $job : "$oexpn"'\(.*\)'"$ojobn"`
    [ $njob = 'same' ] && njob=$job && same=true
    set -- `echo $njob | tr _ ' '`
    nname=$1 ; shift
    while [ $# -gt 1 ] ; do nname=${nname}_$1 ; shift ; done
    njobn=$1
    nexpn=`expr $njob : '\([a-z0-9]*\).*'`
    njmid=`expr $njob : "$nexpn"'\(.*\)'"$njobn"`
    { $dry || $verb; } && echo "in DiFX jobs (same=$same)"
    { $dry || $verb; } && echo "  replacing $oexpn,$ojmid,($oname),$ojobn"
    { $dry || $verb; } && echo "       with $nexpn,$njmid,($nname),$njobn"
}
[ -f "$qa2" ] && {
    old=`tar ztvf $qa2 | cut -d/ -f3 | cut -d. -f1 | sed '/^$/d' | uniq`
    [ $pkg == 'same' ] &&
        echo extracting QA2 tables named $old ||
        echo renaming QA2 tables from $old to $pkg
}

# tell the human
$dry && unload='will unload' || unload='unloading'
[ -f "$difx" ] && echo $unload for DiFX from $difx
[ -f "$qa2" ] && echo $unload for QA2 from $qa2
[ -z "$out" ] && echo no output directory given... && exit 2
echo output to $out
$dry && echo since this is a dry run, stopping now... && exit 0

# create the output directory
cwd=`pwd`
mkdir $out
[ -d $out ] || {
    echo cannot create output directories $out; exit 3; }

# do the DIFX work
[ -f "$difx" ] && {
    echo difx extraction from $difx into $out
    tar -zx -f $difx -C $out
    cd $out
    cat $job.DiFX.path.sed | sed \
        -e 's,.NEWDEST,'`pwd`',' \
        -e 's,.NEWJOB,'$njob',' \
        -e 's,.NEWNAME,'$nname',' \
        -e 's,.NEWEXPN,'$nexpn',' > $njob.sed
    rm $job.DiFX.path.sed
    for df in $oexpn*
    do
        edit=false
        case $df in
        *.calc)     eout=$njob.calc ; edit=true ;;
        *.input)    eout=$njob.input ; edit=true ;;
        *.flag)     eout=$njob.flag ;;
        *.vex*)     eout=${df/$oname/$nname} ; edit=true;;
        *.v2d*)     eout=${df/$oname/$nname} ; edit=true;;
        *.difx)     eout=$njob.difx ;;
        *.sed)      continue ;;
        *)          eout=${df/$job/$njob} ;;
        esac
        $edit && $verb && echo edit: $df " -> " $eout
        $edit && sed -f $njob.sed $df > $eout && rm $df && continue
        $same && continue
        $verb && echo mv: $df $eout
        mv $df $eout
    done
    cd $cwd
}

# do the qa2 work
[ -f "$qa2" ] && {
    echo qa2 extraction from $qa2 into $out
    tar -zx -f $qa2 -C $out
    [ $pkg = 'same' ] || {
        cd $out
        for c in $old.*
        do
            $verb && echo $c " -> " ${c/$old/$pkg}
            mv $c ${c/$old/$pkg}
        done
        cd $cwd
    }
}
$verb && echo && echo finally && echo && ls $out

exit 0
#
# eof
#
