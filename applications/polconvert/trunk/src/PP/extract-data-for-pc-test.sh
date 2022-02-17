#!/bin/bash
#
# A driver script to extract a sample of correlated data for study.
# For fun, we'll try to use getopt(1) which has been around for a while.
#
# defaults for help:
secs=10 bsec=90 asec=90 maxchans=2048 pkg='pkglab'
# usage message
USAGE="$0 [options]

will carve out a useful subset from a DiFX correlation output
along with a useful subset from the corresponding QA2 product
that may be used for testing.

The required options are these:

    -d --difx path      the directory with DiFX products
    -q --qa2 path       the directory with QA2 products
    -o --out path       the directory to hold the results

and one of these

    -t --time string    time of interest (YYYY/MM/DD/HH:MM:SS.sss)
    -j --job string     the difx job input file (expn_jobn.input)

These modify the work done and may be optional:

    -v --verb           if set, chatter about the work (repeatable)
    -y --dry            if set, stop after gathering information
    -k --nuke           if set, delete any existing output directory
    -l --seconds secs   how much DIFX data after time of interest ($secs)
    -b --before secs    how much QA2 data before DIFX is needed ($bsec)
    -a --after secs     how much QA2 data after DIFX is needed ($asec)
    -m --maxchans int   max number (FFT) channels expected in DiFX ($maxchans)
    -p --pkg string     a new label for the QA2 package ($pkg)
    -n --new string     new job name (parsed as expn[...] _ jobn)
    -z --tgz string     if provided, tarballs will be made

and these provide help:

    -h --help           this short help message
    -H --explain        an extended explanatory message

The directory paths may be relative or absolute.  If you supply a time
(-t) a job (-j) supplying that time will be chosen.  If you just supply
a job, then the start is used for the time of interest.  You can make
sensible changes to the outputs with -p and -n if you like.
"
HELP="
This is a driver script to extract a sample of correlated data
from a DiFX output directory and couple it to a similar sample of
QA2 calibration data so that PolConvert may be run on the result.

Since DiFX paths are absolute, some editing is required so that
the sample ends up in a directory with properly edited names.
(This is similar to what difxcopy does.)  This script requires
(in addition to CASA and Python and getopt):

    checkpolconvert.py      (to edit QA2 tables)
    snipDiFX.py             (to edit the SWIN output)

These require a time of interest, specified as YYYY/MM/DD/HH:MM:SS.sss
with a pair of windows in seconds (for QA2 and SWIN) to specify how
much is to be extracted around the time of interest.  The QA2 window
should be larger (perhaps by 90s on each end) than the SWIN window.

Data will be read from the two directories and assembled into a new
directory which should not exist (unless clobber is active).  If the
-z argument is given, tarballs of the QA2 and DiFX data will be made
and placed in the output directory (<name>.{QA2,DiFX},tgz).

For the purposes of this script VEX experiment names are assumed to be
created from a sequence of alphanumeric characters only (which is more
restrictive than the standard).

Extracting across the midnight boundary is not recommended.

For example:
    extract-data-for-pc-test.sh -o example \\
        -t 2021/04/19/01:13:05 -d e21f19-v0-b2a -q 2019.1.00009.CSV.qa2 \\
        -k -n newvex_extra_10 -p qa2snip -z sample

will populate 'example' with data from the e21f19 experiment, clobbering
that directory if it exists.  The new data will be given a new vex name
(newvex) and job number and the QA2 tables will be prefixed qa2snip.  The
tarballs example/sample-*.{QA2,DIFX}.tgz should contain everything.
The DiFX tarball contains a sed script as a reminder if the DiFX tarball
is unpacked to some new location; see unload-data-for-pc-test.sh.
"
# derived from /usr/share/doc/util-linux/getopt-example.bash
short='Hhvkyd:q:o:t:l:b:a:j:m:c:n:z:'
longy='explain,help,verb,nuke,dry,difx:,qa2:,out:,time:,njob:' 
longy="$longy"',seconds:,before:,after:,job:,maxchans:,pkg:,tgz:'
GETOPT=$(getopt -o "$short" -l "$longy" -n $0 -- "$@")
gostat=$?
[ $gostat -ne 0 ] && { echo option parsing error $gostat; exit $gostat; }
eval set -- "$GETOPT" ; unset GETOPT
# set defaults for parsed variables
very=false verb=false user=false tale=false nuke=false dry=false
difx='difx-dir-undefined' qa2='qa2-dir-undefined' when='now'
out='out-dir-undefined'
job='no-job' input='no-input' njob='same' tgz=''
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
'-t'|'--time')      when=$2;    shift 2; continue;;
'-j'|'--job')       job=$2;     shift 2; continue;;
'-l'|'--seconds')   secs=$2;    shift 2; continue;;
'-b'|'--before')    bsec=$2;    shift 2; continue;;
'-a'|'--after')     asec=$2;    shift 2; continue;;
'-m'|'--maxchans')  maxchans=$2;shift 2; continue;;
'-p'|'--pkg')       pkg=$2;     shift 2; continue;;
'-n'|'--new')       njob=$2;    shift 2; continue;;
'-z'|'--tgz')       tgz=$2;     shift 2; continue;;
'--')                             shift;    break;;
*)                  echo parse error>&2;   exit 1;;
esac ; done
$user && echo "Usage: $USAGE" && exit 0
$tale && echo "$HELP" && exit 0

# settle the directory issues
[ -d "$out" ] && $nuke && rm -rf $out && $verb &&
    echo output directory $out existed and was removed
[ -d "$out" ] && { echo dir $out exists--stopping--use -k option; exit 2; }
$verb && echo "taking input from '$difx' and '$qa2' for output to '$out'"

# do we DiFX data?
[ -d "$difx" ] || { echo no DiFX output dir: $difx; exit 3; }

# convert a relative qa2 path to an absolute one
[ -d "$qa2" ] || { echo no QA2 dir: \'$qa2\' does not exist; exit 4; }
pchars=`expr "$qa2" : '^/.*'`
[ $pchars -gt 0 ] || qa2=`pwd`/$qa2
[ -d "$qa2" ] || { echo unable to generate absolute qa2 path ; exit 4 ; }

# identify the supporting codes
ckpc=`type -p checkpolconvert.py`
spdx=`type -p snipDiFX.py`
[ -x "$ckpc" ] || { echo do not have checkpolconvert.py in path; exit 5; }
[ -x "$spdx" ] || { echo do not have snipDiFX.py in path; exit 6; }
casa=`type -p casa`
[ -z "$casa" -a -n "$DIFXCASAPATH" ] && export PATH=$DIFXCASAPATH:$PATH
casa=`type -p casa`
[ -x "$casa" ] || { echo do not have casa in path; exit 7; }
$verb && echo checkpolconvert.py is $ckpc &&
    echo snipDiFX.py is $spdx && echo CASA is $casa

#
# if job is given, get the start time, unless we already have it
# J2000 is 51544
j2kmjd=51544
[ "$when" = 'now' -a ! "$job" = 'no-job' ] && {
    input=$difx/$job
    $verb && echo getting start time and duration from $input
    [ -f $input ] || {
        echo DiFX input file $input is missing and we need it
        echo to find the start time of the job in question...
        exit 8
    }
    set -- `egrep '(^START)|(^EXECUTE)' $input | cut -d: -f2 | tr -d ' '`
    [ $# -eq 3 ] || {
        echo unable to use $input for time information and we need it
        exit 9
    }
    execsecs=$1 mjd=$2 mjdsecs=$3
    ptc="import datetime"
    ptc="$ptc;j2k=datetime.datetime(2000,1,1)"
    ptc="$ptc;pls=datetime.timedelta(days=$mjd-$j2kmjd,seconds=$mjdsecs)"
    ptc="$ptc;jdy=j2k+pls;print(jdy.strftime('when=%Y/%m/%d/%H:%M:%S'))"
    $very && echo job start time-formatting python command: &&
        echo $ptc | sed -e 's/;/; /g' | fold -s | sed -e 's/^/  /'
    when=`python -c "$ptc"`
    $verb && echo job start time $when
    eval $when
    awk "END{if($secs > $execsecs){exit 1;}exit 0;}" < /dev/null || {
        echo "requested duration ($secs) is greater than scan ($execsecs)"
        exit 10
    }
    mjdend=$(($mjdsecs + $execsecs))
}

#
# if the start time is given, figure it out
#
[ -n "$when" ] && {
    # build command for asking python to do the math on time:
    #   checkpolconvert.py needs beg and end values: when + 0,secs +- beg|aft
    #   snipDiFX.py needs a timerange (fd0,fd1) as fractional days of date
    # datetime.timedelta.total_seconds() is floating point total seconds
    #
    [ `expr "$when" : '[^.]*.[^.]*'` -ge 20 ] && { sfmt='%S.%f'; bef='%s';} ||
        { sfmt='%S'; bef='%s.000000'; }
    ptc="import datetime"
    ptc="$ptc;tim='$when';ds=int($secs)"
    ptc="$ptc;fmt='%Y/%m/%d/%H:%M:$sfmt'"
    ptc="$ptc;zd=datetime.datetime.strptime(tim,fmt);day='%Y/%m/%d'"
    ptc="$ptc;z0=datetime.datetime.strptime(tim[0:10],day)"
    ptc="$ptc;j2k=datetime.datetime(2000,1,1);mjdtim=z0-j2k"
    ptc="$ptc;zy=zd-z0;spd=86400.0"
    ptc="$ptc;td=datetime.timedelta(seconds=ds)"
    ptc="$ptc;bsc=datetime.timedelta(seconds=$bsec)"
    ptc="$ptc;asc=datetime.timedelta(seconds=ds+$asec)"
    # so checkpolconvert.py needs beg=(zd-bsc) and end=(zd+asc) with fmt
    # and snipDiFX.py needs range fd0=(yz-td)  and fd1=(yz+td)  as days
    ptc="$ptc;beg=(zd-bsc).strftime(fmt);end=(zd+asc).strftime(fmt)"
    ptc="$ptc;fd0=zy.total_seconds()/spd;fd1=(zy+td).total_seconds()/spd"
    ptc="$ptc;scb=fd0*spd;sce=fd1*spd"
    ptc="$ptc;print('mjdate=%d'%int($j2kmjd+(mjdtim.total_seconds()+0.5)/spd))"
    ptc="$ptc;print('scb=%d'%int(scb),'sce=%d'%int(sce))"
    ptc="$ptc;print('beg=$bef'%beg,'end=$bef'%end)"
    ptc="$ptc;print('fd0=%.15f'%fd0,'fd1=%.15f'%fd1)"
    # ask python, pretty, please....
    $very && echo other times-formatting python command: &&
        echo $ptc | sed -e 's/;/; /g' | fold -s | sed -e 's/^/  /'
    ranges=`python -c "$ptc"`
    $verb && echo time ranges $ranges
    # set the variables
    eval $ranges
}

#
# Hunt down a few QA2 things
# qa2vers is the version of table usage
# qa2label is the prefix
#
qa2vers=`cat $qa2/README.DRIVEPOLCONVERT` || qa2vers=v8
qa2label=`ls $qa2 | sed -e 's/.calibrated.*//' -e 's/.concat.*//' \
    -e '/README/d' -e 's/.qa2.*//' | uniq`
[ -d "$qa2/$qa2label.concatenated.ms.ANTENNA" ] || {
    echo this QA2 package is non-canonical...cannot proceed
    exit 11
}

#
# tell the human what is to happen
#
awks='{h=int(24*$1);m=int((24*$1-h)*60);s=60*(((24*$1-h)*60)-m)}
      s > 59 {m+=1;s-=60;} s<0 {s+=0.00000005} m > 59 {h+=1;m-=60;}
      NR==1{ss=sprintf("  DiFX from       .../%02d:%02d:%09.6f", h,m,s);\
            frm=$1;next}
      NR==2{print ss,sprintf("to        .../%02d:%02d:%09.6f\n"\
            "  with a day range of [%.12f .. %.12f]\n"\
            "  corresponding to sec of mjd of %d .. %d",\
            h,m,s, frm,$1,'$scb,$sce')}'

echo '----------------------------------------------------------------------'
echo Taking DiFX data from $difx "(using snipDiFX.py, $maxchans chans)" and
echo using QA2 tables from $qa2 "(using checkpolconvert.py)"
echo ' 'with label $qa2label and version $qa2vers
echo ' 'for the time ranges on MJD $mjdate:
echo ' ' QA2 from $beg to $end &&
    ( echo $fd0 ; echo $fd1 ) | awk "$awks"
#
# if we don't have the job yet, get it and check duration
#
[ "$job" = 'no-job' ] && {
    for inp in $difx/*.input
    do
        set -- `egrep '(^START)|(^EXECUTE)' $inp | cut -d: -f2 | tr -d ' '`
        [ $# -eq 3 ] || continue
        execsecs=$1 mjd=$2 mjdsecs=$3
        # check date
        [ "$mjd" -eq "$mjdate" ] || continue
        $verb &&
            echo "     $mjdsecs<$scb and $(($mjdsecs + $secs))<$sce" &&
            echo '  'selecting $inp
        # check overlap in seconds
        mjdend=$(($mjdsecs + $execsecs))
        [ "$mjdsecs" -le $scb ] && [ $mjdend -ge $sce ] &&
            input=$inp && job=`basename $inp` && break
    done
}
[ "$scb" -ge "$mjdsecs" -a "$sce" -le "$mjdend" ] ||
    { echo "  (insufficient data to completely fullfill request)"
      echo "  (job from $mjdsecs .. $mjdend, but $scb .. $sce requested)" ; }

#
# And finally a few DiFX things
#
swin=${input/input/difx}
dout=`ls $swin/DIFX*`
[ -f $dout ] || { echo unable to locate DIFX output file; exit 12; }
difxout=`basename $dout`
job=${job/.input/}
# rip apart input filename for expn, junk, jobnumber
set -- `echo $job | tr _ ' '`
oname=$1 ; shift
while [ $# -gt 1 ] ; do oname=${oname}_$1 ; shift ; done
ojobn=$1 ; shift
oexpn=`expr $job : '\([a-z0-9A-Z]*\).*'`
ojmid=`expr $job : "$oexpn"'\(.*\)'"$ojobn"`
[ "$njob" = same ] && njob=$job
set -- `echo $njob | tr _ ' '`
nname=$1 ; shift
while [ $# -gt 1 ] ; do nname=${nname}_$1 ; shift ; done
njobn=$1 ; shift
nexpn=`expr $njob : '\([a-z0-9]*\).*'`
njmid=`expr $njob : "$nexpn"'\(.*\)'"$njobn"`

# and the path edits to make
fullopath=`grep 'OUTPUT FILENAME:' $input | cut -d: -f2 | sed 's/^[ ]*//'`
opath=`dirname $fullopath`
npath=`pwd`/$out
# and round up a list of the usual suspects
dfiles=""
dfcount=0
for df in `ls $difx` ; do case $df in
*.calc)  dfiles="$dfiles $df" ;;
*.flag)  dfiles="$dfiles $df" ;;
*.im*)   dfiles="$dfiles $df" ;;
*.input) dfiles="$dfiles $df" ;;
*.vex*)  dfiles="$dfiles $df" ;;
*.v2d*)  dfiles="$dfiles $df" ;;
*.difx)  ;;
$oexpn*) dfiles="$dfiles $df" ;;
*)       continue ;;
esac ; dfcount=$(($dfcount + 1)); done

[ "$dfcount" -lt 6 ] && {
    echo only $dfcount files present, which is likely not enough ; exit 13 ; }

cwd=`pwd`
echo using job $job, with input $input
echo '  'difx data $dout
echo "  (and data $difxout with $dfcount other files)"
echo "  replacing $oexpn,$ojmid,($oname),$ojobn"
echo "       with $nexpn,$njmid,($nname),$njobn"
echo "  and $opath -> $cwd/$out"
echo Doing the delicate surgery into $out using these commands:
echo '----------------------------------------------------------------------'

# snipDIFX.py arguments
snipdir=$out/$njob.difx
snipout=$out/$njob.difx/$difxout
spdxargs="-t $fd0,$fd1 -c $maxchans -i $input -- $dout $snipout"
$nuke && spdxargs="-k $spdxargs"
$verb && spdxargs="-v $spdxargs"

# checkpolconvert.py arguments
ckpcargs="-r -d $qa2 -q $qa2vers -l $qa2label -c $pkg -b $beg -e $end"
$nuke && ckpcargs="-k $ckpcargs"
$verb && ckpcargs="-v $ckpcargs"

# share the snipDIF command line
echo $spdx $spdxargs |\
    fold -s | sed -e '2,10s/^/  /' -e 's/$/ \\/' -e '$s/ \\//'

# share the checkpolconvert command line
echo $ckpc $ckpcargs |\
    fold -s | sed -e '2,10s/^/  /' -e 's/$/ \\/' -e '$s/ \\//'
echo '----------------------------------------------------------------------'

$dry && echo since this is a dry run, stopping now... && exit 0

$verb && echo Doing the QA2 work...
mkdir $out
[ -d $out ] || {
    echo cannot create output directories $out; exit 14; }
cd $out
$verb && echo ' '...in `pwd`
$ckpc $ckpcargs 2>&1 > ./checkpolconvert.out && echo ...succeeded
echo ' 'removing links &&
    for lnk in $qa2label.* ; do [ -h $lnk ] && rm $lnk ; done
cp -p $qa2/README.DRIVEPOLCONVERT .
[ -n "$tgz" ] && {
    tar zcf $tgz-$pkg.QA2.tgz ./$pkg.* README.DRIVEPOLCONVERT
    echo $out/$tgz-$pkg.QA2.tgz contains extracted QA2 data
}
cd $cwd

# make the DiFX dir--these should not exist at this point (see above)
$verb && echo Doing the DiFX work... &&
    echo ' 'creating $out and $snipdir &&
    echo ' 'new FILE path $npath
mkdir $snipdir
[ -d $out -a $snipdir ] || {
    echo cannot create output directories $snipdir; exit 15; }
for df in $dfiles
do
    edit=false
    extnjn=`expr $df : "${oname}[.]\(.*\)"`
    extwjn=`expr $df : "${oname}_$ojobn[.]\(.*\)"`
    case $df in
    *.calc)  eout=$out/$njob.calc ; edit=true ;;
    *.input) eout=$out/$njob.input ; edit=true ;;
    *.flag)  eout=$out/$njob.flag ;;
    *.vex*)  eout=$out/$nname.$extnjn ; edit=true ;;
    *.v2d)   eout=$out/$nname.$extnjn ; edit=true ;;
    *)       eout=$out/$njob.$extwjn ;;
    esac
    $edit && $verb && echo ' 'edit: $df \'$extnjn\' \'$extwjn\' $eout
    $edit || { $verb && echo ' 'copy: $df \'$extnjn\' \'$extwjn\' $eout ; }
    $edit || cp -p $difx/$df $eout
    $edit && sed \
        -e "/FILE/s,$opath,$npath," \
        -e "/FILE/s,$job,$njob," \
        -e "s,$oname,$nname," \
        -e "s,$oexpn,$nexpn," $difx/$df > $eout
done
$verb && echo running snipDiFX.py...
$spdx $spdxargs |& uniq -c > $out/snipDiFX.out && echo ' '...succeeded.
$verb && ls -l $snipout
cat > $out/$njob.DiFX.path.sed <<EOF
# Edits for .calc, .input, .vex.obs and .v2d
# that are needed to relocate or rename;
# other files may simply be copied
/FILE/s,$npath,\$NEWDEST,
/FILE/s,$njob,\$NEWJOB,
s,$nname,\$NEWNAME,
s,$nexpn,\$NEWEXPN,
EOF
[ -n "$tgz" ] && {
    cd $out
    tar zcf ../$tgz-$njob.DiFX.tgz $njob.* $nname.*
    mv ../$tgz-$njob.DiFX.tgz .
    cd ..
    echo $out/$tgz-$njob.DiFX.tgz contains extracted DiFX data
}

exit 0
#
# eof
#
