#!/bin/bash
#
# Script to generate tarball archives
#
USAGE="$0 [options]

is used to generate the tarballs for the EHTC archive.  Here the
options are all ...=... and many of them need to be supplied.
The main options are (with defaults in parentheses)

    dry=true|false    to do it or to just say it (false)
    exp=<exp>         experiment name (required)
    vers=<vers>       processing version (required)
    subv=<subv>       and subversion (1)
    dest=<dir>        the destination directory (.)
    src=<dir>         the directory holding data (.)
    verb=true|false   be chatty about our work (true)

and one of the simple tar targets,

    tar=dxin|swin|fits|hops|pcin|pcqk|pcal|logs

which specify which tarballs are to be made:

    dxin    DiFX input files
    swin    DiFX (SWIN) output files
    fits    difx2fits outputs
    hops    difx2mark4 outputs
    pcin    polconvert inputs (if ALMA)
    pcqk    polconvert quick-look outputs (if ALMA)
    pcal    polconvert calibration table tarball (if ALMA)
    logs    all tar creation logs

Or, for convenience,

    tar=no-alma     does:   dxin swin fits hops
    tar=no-hops     does:   dxin swin fits
    tar=pre-alma    does:   dxin swin
    tar=post-alma   does:   pcin fits hops pcqk pcal

The no-alma case is presumed to have been processed in one directory;
the no-hops case is if d2m4 was already run (and collected from
multiple difx job sequences); and for the ALMA cases, where PolConvert is
used the pre-alma and post-alma cases are presumed to have been processed
in two separate directories (hence the two steps).  Some of the tarballs
require additional information which is also supplied via options:

    job=<job>       if the v2d job name isn't the experiment name
    expn=<nnnn>	    the assigned 4-digit HOPS experiment number
    d2m4=true|false run difx2mark4 with \$exp.codes (true)
    d2ft=true|false run difx2fits (true)
    copy=true|false copy the tarballs to dest (true)
    nuke=true|false remove working and dest products (false)

The d2?? options presume you want to process all input files in \$src
and will refuse to run if they find that this has already been done.
The nuke option is provided as an easy way to remove tarballs (working
or installed) if something went wrong and you just want to try again.
It will not remove the difx2mark4 or difx2fits output, nor the
polconvert calibration tarball (*APP_DELIVERABLES*).

The single argument 'examples' will provide some suggestions.
The single argument 'other' will provide some extra special use args.
"
EXAMPLES="Standard delivery at the Correlators is to subdirectories
(as sensible) of one of these:

    test=/data-sc04/EHT_ARCHIVE/SpecialUsage
    hays=/data-sc04/EHT_ARCHIVE/Hays_Output
    bonn=/some-such-directory/Bonn_Output

Since there will be many tarballs per observational campaign, it makes
some sense to put the tarballs in some sensible directory hierarchy.

Some specific examples are given with an argument of

    examples-mike       # 2016 eht non-alma session
    examples-alma       # 2016 alma sessions
    examples-generic    # other examples
"
EXMIKE="For a non-ALMA case these might work for you:

    # 1mm 2016 production
    dir=/data-sc05/difxoper
    $0 \\
        exp=e16n04 vers=prepass subv=1h expn=3553 d2m4=false job=h \\
        dest=\$hays/e16n04 src=\$dir/e16n04_prepass tar=no-hops
    $0 \\
        exp=e16n04 vers=prepass subv=1i expn=3553 d2m4=false job=i \\
        dest=\$hays/e16n04 src=\$dir/e16n04_prepass tar=no-hops
    $0 \\
        exp=e16n04 vers=prepass subv=1 expn=3553 d2m4=false d2ft=false \\
        dest=\$hays/e16n04 src=\$dir/e16n04_prepass tar=hops

    # 1mm 2016 production
    dir=/data-sc05/difxoper
    $0 \\
        exp=e16n05 vers=prepass subv=1h expn=3554 d2m4=false job=h \\
        dest=\$hays/e16n05 src=\$dir/e16n05_prepass tar=no-hops
    $0 \\
        exp=e16n05 vers=prepass subv=1i expn=3554 d2m4=false job=i \\
        dest=\$hays/e16n05 src=\$dir/e16n05_prepass tar=no-hops
    $0 \\
        exp=e16n05 vers=prepass subv=1 expn=3554 d2m4=false d2ft=false \\
        dest=\$hays/e16n05 src=\$dir/e16n05_prepass tar=hops

"
EXALMA="For ALMA cases, something like these might work for you (note this
will run difx2mark4 as well as difx2fits):
    
    # 1mm
    dir=/data-sc05/gbc
    $0 \\
        exp=e16b08 vers=5-pc7 subv=4 expn=3557 \\
        dest=\$test/test src=\$dir/alma-april-band6-5 tar=pre-alma
    $0 \\
        exp=e16b08 vers=5-pc7 subv=4 expn=3557 \\
        dest=\$test/test src=\$dir/alma-april-band6-5-pc7 tar=post-alma

    # 3mm
    dir=/data-sc04/gbc
    $0 \\
        exp=bm452c vers=3-pc7x subv=4 expn=3566 \\
        dest=\$test/band3 src=\$dir/alma-vlba-band3-3 tar=pre-alma
    $0 \\
        exp=bm452c vers=3-pc7x subv=4 expn=3566 \\
        dest=\$test/band3 src=\$dir/alma-vlba-band3-3-pc7x tar=post-alma
"
EXGEN="To remove tarballs (in case you don't like what was installed):

    $0 ... dry=true nuke=true

which will remove the tarballs and associated logs from the working
directory as well as the destination directory.
"
OTHER="Various additional arguments

    over=true|false     --override-version on difx2{mark4|fits} (false)
    jobs <list>         terminate argument processing and consider
                        everything else to be a job number (or job.input)
"

# defaults
dry=false
exp=noexp
vers=novers
subv=1
dest=.
src=.
job=nojob
tar=notar
expn=0000
d2m4=true
d2ft=true
copy=true
verb=true
nuke=false
over=false

args="$@"

[ $# -eq 0 ] && set -- 'help'

# eat up the arguments
while [ $# -gt 0 ] ; do
case $1 in
-h)      echo "$USAGE" ; exit 0 ;;
*help*)  echo "$USAGE" ; exit 0 ;;
*e*mike) echo "$EXMIKE" ; exit 0 ;;
*e*alma) echo "$EXALMA" ; exit 0 ;;
*e*gen*) echo "$EXGEN" ; exit 0 ;;
*exam*)  echo "$EXAMPLES" ; exit 0 ;;
other)   echo "$OTHER" ; exit 0 ;;
dry=*)   eval "$1" ;;
exp=*)   eval "$1" ;;
vers=*)  eval "$1" ;;
subv=*)  eval "$1" ;;
dest=*)  eval "$1" ;;
src=*)   eval "$1" ;;
tar=*)   eval "$1" ;;
verb=*)  eval "$1" ;;
job=*)   eval "$1" ;;
expn=*)  eval "$1" ;;
d2m4=*)  eval "$1" ;;
d2ft=*)  eval "$1" ;;
copy=*)  eval "$1" ;;
nuke=*)  eval "$1" ;;
over=*)  eval "$1" ;;
jobs)    shift ; break ;;
esac ; shift ; done

# finish up the jobs list
jobs=''
for j in $@
do jobs="$jobs "`echo $j | sed 's/.input//'` ; done
echo jobs is $jobs

# marching orders
$verb && echo '' && echo '' && echo $0 $args | fold && echo ''
# check for sanity
[ -d $dest ] || { mkdir -p $dest && $verb && echo created $dist ; }
[ -d $dest ] || { echo dir $dest does not exist, make it ; exit 1 ; }
[ -d $src  ] || { echo dir $src  does not exist ; exit 1 ; }
[ "$exp" = 'noexp'   ] && { echo exp must be supplied ; exit 1 ; }
[ "$vers" = 'novers' ] && { echo vers must be supplied ; exit 1 ; }
[ -z "$subv" ] && { echo subv must be supplied ; exit 1 ; }
[ "$dry" = 'true' -o "$dry" = 'false' ] || {
    echo dry must be true or false ; exit 1; }
[ "$verb" = 'true' -o "$verb" = 'false' ] || {
    echo verb must be true or false ; exit 1; }
[ "$d2m4" = 'true' -o "$d2m4" = 'false' ] || {
    echo d2m4 must be true or false ; exit 1; }
[ "$d2ft" = 'true' -o "$d2ft" = 'false' ] || {
    echo d2ft must be true or false ; exit 1; }
[ "$copy" = 'true' -o "$copy" = 'false' ] || {
    echo copy must be true or false ; exit 1; }
[ "$nuke" = 'true' -o "$nuke" = 'false' ] || {
    echo nuke must be true or false ; exit 1; }
[ "$over" = 'true' -o "$over" = 'false' ] || {
    echo over must be true or false ; exit 1; }
$d2m4 && [ "$expn" = '0000' ] && { echo d2m4 is true, need expn; exit 1; }
[ "$job" = 'nojob' ] && job=$exp
EXP=`echo $exp | tr a-z A-Z`
com1="nuke=$nuke exp=$exp vers=$vers subv=$subv"
com2="verb=$verb dest=$dest"
com3="dry=$dry src=$src"
com4="copy=$copy job=$job expn=$expn EXP=$EXP d2m4=$d2m4 d2ft=$d2ft"
com5="over=$over jobs $jobs"

# verify write permissions in the work directory (for tar creation)
workdir=`pwd`
echo 'hi mom' > test-file.$$
[ -s test-file.$$ ] || { echo no write permission in $workdir ; exit 2; }
rm -f test-file.$$
# verify write permissions in the source directory (for d2m4 and d2ft)
cd $src
srcdir=`pwd`
echo 'hi mom' > test-file.$$
[ -s test-file.$$ ] || { echo no write permission in $srcdir ; exit 2; }
rm -f test-file.$$
# verify write permissions in the dest directory (for copying)
cd $workdir
cd $dest
destdir=`pwd`
echo 'hi mom' > test-file.$$
[ -s test-file.$$ ] || { echo no write permission in $destdir ; exit 2; }
rm -f test-file.$$
# get joblist and see if source is dest
cd $srcdir
# either user supplied $jobs from above or else everything
[ -z "$jobs" ] && jobs=`ls ${job}*.input | sed 's/.input$//'`
set -- $jobs
[ $# -eq 0 ] && { $d2m4 || $d2ft ; } && { echo no jobs found ; exit 2; }
job1=$1
[ $# -ge 1 ] && shift $(($# - 1))
jobn=$1
$copy && [ "$srcdir" = "$destdir" ] && { echo source is dest: `pwd` ; exit 2; }

{ $d2m4 || $d2ft ; } && {
    for j in $jobs
    do [ -f $j.input ] || { echo $j.input missing; exit 2; } ; done ; }

d2m4exec=`type -p difx2mark4`
d2ftexec=`type -p difx2fits`
[ -n "$d2m4exec" ] || { echo difx2mark4 not found in path ; exit 2; }
[ -n "$d2ftexec" ] || { echo difx2fits not found in path ; exit 2; }
[ -x "$d2m4exec" ] || { echo difx2mark4 is not executable ; exit 2; }
[ -x "$d2ftexec" ] || { echo difx2fits is not executable ; exit 2; }
$over && ov=--override-version || ov=''

$verb && echo Sanity checks passed, proceeding with arguments &&
    echo '        ' $com1 \\ &&
    echo '        ' $com2 \\ &&
    echo '        ' $com3 \\ &&
    echo '        ' $com4 \\ &&
    echo '        ' $com5
$verb && echo Using jobs $job1 .. $jobn "($jobs)"
$verb && echo Working in $srcdir
$verb && echo Tar output to $workdir
$verb && echo Delivery to to $destdir

#
# Proceed to the individual tar commands.  The case below defines
# the tarball names and contents or invokes this script for members
# of a collection.
#
dotar=false
work=none
tarname=''
content=''
case $tar in
dxin)
    $verb && echo gathering DiFX inputs in `pwd`
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-dxin.tar
    $nuke && rm -f $workdir/$tarname && rm -f $dest/$tarname &&
             rm -f $workdir/$tarname.log
    ### FIXME content
    content1='*.flist* *.v2d* *.joblist* *codes *vex.obs'
    content2=' ${job}_*.{input,calc,errs,flag,im,machines,threads,difxlog}'
    content=$content1$content2
    ;;
swin)
    $verb && echo gathering DiFX output in `pwd`
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-swin.tar
    $nuke && rm -f $workdir/$tarname && rm -f $dest/$tarname &&
             rm -f $workdir/$tarname.log
    ### FIXME content
    content='${job}_*.difx'
    ;;
fits)
    fits=${exp}-${vers}-$subv.fits
    $verb && echo making FITS in `pwd`/$fits
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-fits.tar
    $nuke && rm -rf $fits
    $nuke && rm -f $workdir/$tarname && rm -f $dest/$tarname &&
             rm -f $workdir/$tarname.log
    content="$fits"
    $d2ft || [ -d $fits ] || { echo no FITS output dir $fits; exit 3; }
    $d2ft && work=fits
    ;;
hops)
    $verb && echo making HOPS in `pwd`/$expn
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-hops.tar
    $nuke && rm -rf $expn
    $nuke && rm -f $workdir/$tarname && rm -f $dest/$tarname &&
             rm -f $workdir/$tarname.log
    content="$expn"
    $d2m4 || [ -d "$expn" ] || { echo No HOPS output $expn to tar; exit 3; }
    $d2m4 && work=hops
    ;;
pcin)
    $verb && echo gathering PolConvert inputs in `pwd`
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-pcin.tar
    $nuke && rm -f $workdir/$tarname && rm -f $dest/$tarname &&
             rm -f $workdir/$tarname.log
    ### FIXME content
    content1='SourceList.txt SideBand.txt Jobs.txt'
    content2=" ${job}*.{input,calc,flag,im}"
    content3=" ${exp}*.codes ${exp}*.conf ${exp}*.vex.obs"
    content=$content1$content2$content3
    work=pcin
    ;;
pcqk)
    $verb && echo gathering PolConvert quick-look in `pwd`
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-pcqk.tar
    $nuke && rm -f $workdir/$tarname && rm -f $dest/$tarname &&
             rm -f $workdir/$tarname.log
    content1="$exp*polcon*/FR*/[AR]*png *.txt"
    content2=" $exp*polcon*/POLCONVERT.GAINS"
    content3=" $exp*polcon*/POLCONVERT_STATION1.ANTAB"
    content=$content1$content2$content3
    ;;
pcal)
    $verb && echo gathering PolConvert calibration `pwd`
    pcal=`ls *APP_DELIVERABLES*`
    [ -f "$pcal" ] || { echo "*APP_DELIVERABLES*" not found ; exit 3 ; }
    content=$pcal
    $nuke && [ -f $dest/$content ] &&
        echo $dest already exists and you have nuke=true &&
        echo FIX with: rm -f $dest/$content
    ;;
logs)
    $verb && echo gathering tar creation logs calibration logs
    cd $workdir
    ### FIXME content
    echo FIXME
    ;;
no-alma)
    $verb && echo doing no-alma case in `pwd`
    cd $workdir
    $0 tar=dxin $com1 $com2 $com3 $com4 $com5 || { echo dxin failed ; exit 3; }
    $0 tar=swin $com1 $com2 $com3 $com4 $com5 || { echo swin failed ; exit 3; }
    $0 tar=fits $com1 $com2 $com3 $com4 $com5 || { echo fits failed ; exit 3; }
    $0 tar=hops $com1 $com2 $com3 $com4 $com5 || { echo hops failed ; exit 3; }
    exit 0
    ;;
no-hops)
    $verb && echo doing no-hops case in `pwd`
    cd $workdir
    $0 tar=dxin $com1 $com2 $com3 $com4 $com5 || { echo dxin failed ; exit 3; }
    $0 tar=swin $com1 $com2 $com3 $com4 $com5 || { echo swin failed ; exit 3; }
    $0 tar=fits $com1 $com2 $com3 $com4 $com5 || { echo fits failed ; exit 3; }
    exit 0
    ;;
pre-alma)
    $verb && echo doing pre-alma case in `pwd`
    cd $workdir
    $0 tar=dxin $com1 $com2 $com3 $com4 $com5 || { echo dxin failed ; exit 3; }
    $0 tar=swin $com1 $com2 $com3 $com4 $com5 || { echo swin failed ; exit 3; }
    exit 0
    ;;
post-alma)
    $verb && echo doing post-alma case in `pwd`
    cd $workdir
    $0 tar=pcin $com1 $com2 $com3 $com4 $com5 || { echo pcin failed ; exit 3; }
    $0 tar=fits $com1 $com2 $com3 $com4 $com5 || { echo fits failed ; exit 3; }
    $0 tar=hops $com1 $com2 $com3 $com4 $com5 || { echo hops failed ; exit 3; }
    $0 tar=pcqk $com1 $com2 $com3 $com4 $com5 || { echo pcqk failed ; exit 3; }
    $0 tar=pcal $com1 $com2 $com3 $com4 $com5 || { echo pcal failed ; exit 3; }
    exit 0
    ;;
*) cat <<....EOF
    dxin    DiFX input files
    swin    DiFX (SWIN) output files
    fits    difx2fits outputs
    hops    difx2mark4 outputs
    pcin    polconvert inputs (if ALMA)
    pcqk    polconvert quick-look outputs (if ALMA)
    pcal    polconvert calibration table tarball (if ALMA)
    logs    all tar creation logs

    tar=no-alma     does:   dxin swin fits hops
    tar=no-hops     does:   dxin swin fits
    tar=pre-alma    does:   dxin swin
    tar=post-alma   does:   pcin fits hops pcqk pcal
....EOF
    exit 3
    ;;
esac

# actually do some tar-dependent work in a few cases
case $work in
none)
    ;;
pcin)
    $verb && echo making $content1
    [ -f SourceList.txt ] || $dry ||
        grep SOURCE.0.N ${job}*calc > SourceList.txt
    [ -f SideBand.txt ] || $dry ||
        grep 'SIDEBAND 32' ${job}*input > SideBand.txt
    [ -f Jobs.txt ] || $dry ||
        sort SourceList.txt SideBand.txt | paste - - |\
	awk '{printf "%s %-10s %s\n", $1,$4,$7}' |\
	sed 's/.calc:SOURCE//' > Jobs.txt
    ls -l SourceList.txt SideBand.txt Jobs.txt
    ;;
fits)
    $verb && echo running difx2fits and moving output to fits directory
    fog=$fits/difx2fits-${exp}-${vers}-$subv.log
    [ -d "$fits" ] && {
        echo `pwd`/$fits exists, but d2ft is true;
        echo FIX with:  rm -rf `pwd`/$fits
        exit 4; }
    $dry && {
        echo mkdir $fits
        echo $d2ftexec $ov $jobs \> $fog
        echo $EXP* $fits
    } || {
        mkdir $fits
        $verb && echo follow difx2fits with: &&
            echo '  'tail -n +1 -f `pwd`/$fog
        echo $d2ftexec $ov $jobs > $fog
        echo =================== >> $fog
        $d2ftexec $ov $jobs >> $fog 2>&1 || {
            echo difx2fits failed; exit 4; }
        mv $EXP* $fits
        $verb && echo -n disk usage on fits: && du -sh $fits
    }
    ;;
hops)
    $verb && echo running difx2mark4 for $expn in `pwd` with $exp.codes
    dog=$expn/difx2mark4-${exp}-${vers}-$subv.log
    [ -d "$expn" ] && {
        echo `pwd`/$expn exists, but d2m4 is true;
        echo FIX with:  rm -rf `pwd`/$expn
        exit 4; }
    [ -f "$exp.codes" ] || {
        [ -f "codes" ] && {
            echo    cp -p codes $exp.codes &&
            $dry || cp -p codes $exp.codes
        }
    }
    $dry && {
        echo mkdir $expn
        echo $d2m4exec $ov -e $expn -s $exp.codes $jobs \> $dog
    } || {
        mkdir $expn
        $verb && echo follow difx2mark4 with: &&
            echo '  'tail -n +1 -f `pwd`/$dog
        echo $d2m4exec $ov -e $expn -s $exp.codes $jobs > $dog
        echo ========================================== >> $dog
        $d2m4exec $ov -e $expn -s $exp.codes $jobs >> $dog 2>&1 || {
            echo difx2mark4 failed; exit 4; }
        $verb && echo -n disk usage on ${expn}: && du -sh $expn
    }
    ;;
esac

# for the tarball cases, actually make the tarball
$dotar && [ -n "$tarname" -a -n "$content" ] && (
    $verb && echo Making $tarname, see $workdir/$tarname.log
    rm -f $workdir/$tarname $workdir/$tarname.log
    exec > $workdir/$tarname.log 2>&1
    echo \
    tar -c --ignore-failed-read -f $workdir/$tarname $content
    eval \
    tar -c --ignore-failed-read -f $workdir/$tarname $content
    [ -s "$workdir/$tarname" ] || {
        echo "tarball '$tarname' is empty"; exit 5; }
    true
) || [ "$tar" = 'pcal' ] || {
    $verb && echo "Not making tarball '$tarname', so nothing to copy"
    copy=false
}

# and finally copy it to the destination
[ "$tar" = 'pcal' ] && {
    tarname=$content
    workdir=$srcdir
}
# general tarball install rule
$copy && {
    [ -f $dest/$tarname ] && {
        echo destination tarball $dest/$tarname exists; exit 6; }
    echo -n cp -p $workdir/$tarname $dest ... &&
            cp -p $workdir/$tarname $dest && echo ok
    [ -f $dest/$tarname ] && ls -lh $dest/$tarname
    $nuke && [ -f $dest/$tarname ] && rm -f $workdir/$tarname
}

exit 0
#
# eof
#
