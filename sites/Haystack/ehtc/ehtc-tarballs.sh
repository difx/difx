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

    tar=dxin|swin|fits|hops|hmix|pcin|pcqk|pcal|4fit

which specify which tarballs are to be made:

    dxin    DiFX input files
    swin    DiFX (SWIN) output files
    fits    difx2fits outputs
    hops    difx2mark4 outputs following polconvert
    hmix    difx2mark4 outputs prior to polconvert
    pcin    polconvert inputs (if ALMA)
    pcqk    polconvert quick-look outputs (if ALMA)
    pcal    polconvert calibration table tarball (if ALMA)
    4fit    correlator fourfit output (pc'd; type 200s + alist)

Or, for convenience,

    tar=post-corr   does:   dxin swin
    tar=no-alma     does:   dxin swin fits hops
    tar=no-hops     does:   dxin swin fits
    tar=pre-alma    does:   dxin swin hmix
    tar=post-alma   does:   pcin fits hops pcqk

(Note pcal and 4fit probably don't make sense in these groupings.)
The 4fit option requires 2 passes; one to build an \$expn dir to
run fourfit in, and a second pass to create the tarball.  You may
repeat this (see \$flab below) non-destructively.

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
    save=true|false save products that might get nuked (false)
    label=          an additional token to be included in tarnames
    flab=           an additional version label for re-fourfitting

The d2?? options presume you want to process all input files in \$src
and will refuse to run if they find that this has already been done.
The nuke option is provided as an easy way to remove tarballs (working
or installed) if something went wrong and you just want to try again.
It will not remove the difx2mark4 or difx2fits output, nor the
polconvert calibration tarball (*APP_DELIVERABLES*).  The save option
refers to directories used by the HOPS and FITS processing; if true,
these directories will be labelled so that they not likely to be
nuked on a subsequent run.  The label (if non-empty) can be used to
label tarballs of subsets of processing (e.g. by project-source).
Finally flab can be used to support re-fourfitting (e.g. with a new
fourfit control file) and is only used in the 4fit option.  The saved
dir and tarball will be named '4fit\$flab' so name wisely.

Typically the label would be <project>-<source>.

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
save=false
label=''
flab=''
target=none

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
save=*)  eval "$1" ;;
label=*) eval "$1" ;;
flab=*)   eval "$1" ;;
target=*) eval "$1" ;;
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
[ -d $dest ] || { mkdir -p $dest && $verb && echo created $dest ; }
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
[ -z "$label" ] && label='any'
[ "$target" = none -a "$tar" = 4fit ] && {
    echo you must specify a target for 4fit tar option; exit 1;
}

# variables passed to group tasks
com1="nuke=$nuke exp=$exp vers=$vers subv=$subv"
com2="verb=$verb dest=$dest"
com3="dry=$dry src=$src"
com4="copy=$copy job=$job expn=$expn EXP=$EXP d2m4=$d2m4 d2ft=$d2ft"
com5="over=$over save=$save label=$label target=$target flab=$flab jobs $jobs"

# verify write permissions in the work directory (for tar creation)
workdir=`pwd`
echo 'hi mom' > test-file.$$
[ -s test-file.$$ ] || { echo no write permission in $workdir ; exit 2; }
rm -f test-file.$$
# verify write permissions in the source directory (for d2m4 and d2ft)
cd $src
srcdir=`pwd`
#echo 'hi mom' > test-file.$$
#[ -s test-file.$$ ] || { echo no write permission in $srcdir ; exit 2; }
#rm -f test-file.$$
# verify write permissions in the dest directory (for copying)
cd $workdir
[ -d logs ] || mkdir logs
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
# sanity check for input data for these tasks
{ $d2m4 || $d2ft ; } && {
    for j in $jobs
    do [ -f $j.input ] || { echo $j.input missing; exit 2; } ; done ; }
{ $d2m4 || $d2ft ; } && {
    for j in $jobs
    do [ -d $j.difx ] || { echo $j.difx missing; exit 2; } ; done ; }

d2m4exec=`type -p difx2mark4`
d2ftexec=`type -p difx2fits`
fourfit=`type -p fourfit`
[ -n "$d2m4exec" ] || { echo difx2mark4 not found in path ; exit 2; }
[ -n "$d2ftexec" ] || { echo difx2fits not found in path ; exit 2; }
[ -n "$fourfit"  ] || { echo fourfit not found in path ; exit 2; }
[ -x "$d2m4exec" ] || { echo difx2mark4 is not executable ; exit 2; }
[ -x "$d2ftexec" ] || { echo difx2fits is not executable ; exit 2; }
[ -x "$fourfit"  ] || { echo fourfit is not executable ; exit 2; }
$over && ov=--override-version || ov=''
delete=''

$verb && echo Sanity checks passed, proceeding with arguments &&
    echo '        ' $com1 \\ &&
    echo '        ' $com2 \\ &&
    echo '        ' $com3 \\ &&
    echo '        ' $com4 \\ &&
    echo '        ' $com5
$verb && echo Using jobs $job1 .. $jobn "($jobs)"
$verb && echo Source is $srcdir
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
    tarname=${exp}-${vers}-$subv-$label-dxin.tar
    $nuke && rm -f $workdir/$tarname && rm -f $destdir/$tarname &&
             rm -f $workdir/logs/$tarname.log
    content1='*.flist* *.v2d* *.joblist* *codes *vex.obs'
    content2=''
    for j in $jobs
    do
      content2="$content2 ${j}.{input,calc,errs,flag,im}"
      content2="$content2 ${j}.{machines,threads,difxlog}"
    done
    content=$content1$content2
    ;;
swin)
    $verb && echo gathering DiFX output in `pwd`
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-$label-swin.tar
    $nuke && rm -f $workdir/$tarname && rm -f $destdir/$tarname &&
             rm -f $workdir/logs/$tarname.log
    content=''
    for j in $jobs
    do
      content="$content ${j}.difx"
    done
    ;;
fits)
    fits=${exp}-${vers}-$subv-$label.fits
    $verb && echo making FITS in `pwd`/$fits
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-$label-fits.tar
    $nuke && rm -rf $fits
    $nuke && rm -f $workdir/$tarname && rm -f $destdir/$tarname &&
             rm -f $workdir/logs/$tarname.log
    content="$fits"
    $d2ft || [ -d $fits ] || { echo no FITS output dir $fits; exit 3; }
    $d2ft && work=fits
    ;;
hops|hmix)
    $verb && [ $tar = hops ] && echo making HOPS'(post-pc)' in `pwd`/$expn
    $verb && [ $tar = hmix ] && echo making HOPS'(pre-pc)'  in `pwd`/$expn
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-$label-$tar.tar
    $nuke && rm -rf $expn
    $nuke && rm -f $workdir/$tarname && rm -f $destdir/$tarname &&
             rm -f $workdir/logs/$tarname.log
    content="$expn"
    $d2m4 || [ -d "$expn" ] || { echo No HOPS output $expn to tar; exit 3; }
    $d2m4 && work=hops
    ;;
pcin)
    $verb && echo gathering PolConvert inputs in `pwd`
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-$label-pcin.tar
    $nuke && rm -f $workdir/$tarname && rm -f $destdir/$tarname &&
             rm -f $workdir/logs/$tarname.log
    content1="SourceList-$label.txt SideBand-$label.txt Jobs-$label.txt"
    content2=''
    for j in $jobs
    do
      content2=" ${j}.{input,calc,flag,im}"
    done
    content3=" ${exp}*.codes ${exp}*.conf ${exp}*.vex.obs"
    content=$content1$content2$content3
    work=pcin
    ;;
pcqk)
    $verb && echo gathering PolConvert quick-look in `pwd`
    $dry || dotar=true
    tarname=${exp}-${vers}-$subv-$label-pcqk.tar
    $nuke && rm -f $workdir/$tarname && rm -f $destdir/$tarname &&
             rm -f $workdir/logs/$tarname.log
    content=''
    for j in $jobs
    do
      content="$content ${j}*polconvert*/*MATRIX"
      content="$content ${j}*polconvert*/*PEAKS"
      content="$content ${j}*polconvert*/*PLOTS"
      content="$content ${j}*polconvert*/*GAINS"
      content="$content ${j}*polconvert*/*ANTAB"
      content="$content ${j}*polconvert*/PolConvert.log"
    done
    ;;
pcal)
    $verb && echo gathering PolConvert calibration `pwd`
    pcal=`ls *APP_DELIVERABLES*`
    [ -f "$pcal" ] || { echo "*APP_DELIVERABLES*" not found ; exit 3 ; }
    content=$pcal
    $nuke && [ -f $destdir/$content ] &&
        echo $destdir already exists and you have nuke=true &&
        echo FIX with: rm -f $destdir/$content
    ;;
4fit)
    $verb && echo fourfitting in `pwd`
    ffconf=$exp-$vers-$subv.conf
    [ -f $ffconf ] || { echo no config file for fourfitting ; exit 3 ; }
    [ -d $expn ] && work=4fit-4fit || work=4fit-prep
    [ $work = 4fit-prep ] && tarname=''
    [ $work = 4fit-4fit ] &&
        tarname=${exp}-${vers}-$subv-$label-4fit$flab.tar &&
        $nuke && rm -f $workdir/$tarname && rm -f $destdir/$tarname &&
                 rm -f $workdir/logs/$tarname.log
    [ $work = 4fit-4fit ] && $dry || dotar=true
    content="$expn"
    echo 'type of work is' $work and flab is "'$flab'".
    ;;
no-alma)
    $verb && echo doing no-alma case in `pwd`
    cd $workdir
    $0 tar=dxin $com1 $com2 $com3 $com4 $com5 || { echo dxin failed ; exit 3; }
    cd $workdir
    $0 tar=swin $com1 $com2 $com3 $com4 $com5 || { echo swin failed ; exit 3; }
    cd $workdir
    $0 tar=fits $com1 $com2 $com3 $com4 $com5 || { echo fits failed ; exit 3; }
    cd $workdir
    $0 tar=hops $com1 $com2 $com3 $com4 $com5 || { echo hops failed ; exit 3; }
    exit 0
    ;;
no-hops)
    $verb && echo doing no-hops case in `pwd`
    cd $workdir
    $0 tar=dxin $com1 $com2 $com3 $com4 $com5 || { echo dxin failed ; exit 3; }
    cd $workdir
    $0 tar=swin $com1 $com2 $com3 $com4 $com5 || { echo swin failed ; exit 3; }
    cd $workdir
    $0 tar=fits $com1 $com2 $com3 $com4 $com5 || { echo fits failed ; exit 3; }
    exit 0
    ;;
post-corr)
    $verb && echo doing post-corr case in `pwd`
    cd $workdir
    $0 tar=dxin $com1 $com2 $com3 $com4 $com5 || { echo dxin failed ; exit 3; }
    cd $workdir
    $0 tar=swin $com1 $com2 $com3 $com4 $com5 || { echo swin failed ; exit 3; }
    exit 0
    ;;
pre-alma)
    $verb && echo doing pre-alma case in `pwd`
    cd $workdir
    $0 tar=dxin $com1 $com2 $com3 $com4 $com5 || { echo dxin failed ; exit 3; }
    cd $workdir
    $0 tar=swin $com1 $com2 $com3 $com4 $com5 || { echo swin failed ; exit 3; }
    cd $workdir
    $0 tar=hmix $com1 $com2 $com3 $com4 $com5 || { echo hmix failed ; exit 3; }
    exit 0
    ;;
post-alma)
    $verb && echo doing post-alma case in `pwd`
    cd $workdir
    $0 tar=pcin $com1 $com2 $com3 $com4 $com5 || { echo pcin failed ; exit 3; }
    cd $workdir
    $0 tar=fits $com1 $com2 $com3 $com4 $com5 || { echo fits failed ; exit 3; }
    cd $workdir
    $0 tar=hops $com1 $com2 $com3 $com4 $com5 || { echo hops failed ; exit 3; }
    cd $workdir
    $0 tar=pcqk $com1 $com2 $com3 $com4 $com5 || { echo pcqk failed ; exit 3; }
    exit 0
    ;;
*) cat <<....EOF
    dxin    DiFX input files
    swin    DiFX (SWIN) output files
    fits    difx2fits outputs
    hops    difx2mark4 outputs
    hmix    difx2mark4 outputs prior to polconvert
    pcin    polconvert inputs (if ALMA)
    pcqk    polconvert quick-look outputs (if ALMA)
    pcal    polconvert calibration table tarball (if ALMA)
    4fit    correlator fourfit output (pc'd; type 200s + alist)

    tar=post-corr   does:   dxin swin
    tar=no-alma     does:   dxin swin fits hops
    tar=no-hops     does:   dxin swin fits
    tar=pre-alma    does:   dxin swin hmix
    tar=post-alma   does:   pcin fits hops pcqk pcal
....EOF
    exit 3
    ;;
esac

# actually do some tar-dependent work in a few cases prior to the tar
case $work in
none)
    ;;
pcin)
    $verb && echo making $content1
    [ -f SourceList-$label.txt ] || $dry ||
        grep SOURCE.0.N ${job}*calc > SourceList-$label.txt
    [ -f SideBand-$label.txt ] || $dry ||
        grep 'SIDEBAND 32' ${job}*input > SideBand-$label.txt
    [ -f Jobs-$label.txt ] || $dry ||
        sort SourceList-$label.txt SideBand-$label.txt | paste - - |\
	awk '{printf "%s %-10s %s\n", $1,$4,$7}' |\
	sed 's/.calc:SOURCE//' > Jobs-$label.txt
    ls -l SourceList-$label.txt SideBand-$label.txt Jobs-$label.txt
    delete="SourceList-$label.txt SideBand-$label.txt Jobs-$label.txt"
    ;;
fits)
    $verb && echo running difx2fits and moving output to fits directory
    fog=$fits/difx2fits-${exp}-${vers}-$subv.log
    [ -d "$fits" ] && {
        echo `pwd`/$fits exists, but d2ft is true;
        echo FIX with:  rm -rf `pwd`/$fits
        exit 4;
    }
    $dry && {
        echo mkdir $fits
        echo $d2ftexec $ov $jobs \> $fog
        echo $EXP* $fits
    } || {
        mkdir $fits
        $save && savename=$fits.save
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
hops|hmix)
    $verb && echo running difx2mark4 for $expn in `pwd` with $exp.codes
    dog=$expn/difx2mark4-${exp}-${vers}-$subv.log
    [ -d "$expn" ] && {
        echo `pwd`/$expn exists, but d2m4 is true;
        echo FIX with:  rm -rf `pwd`/$expn
        exit 4;
    }
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
        $save && savename=$exp-$vers-$subv-$label.$expn.save
        $verb && echo follow difx2mark4 with: &&
            echo '  'tail -n +1 -f `pwd`/$dog
        echo $d2m4exec $ov -e $expn -s $exp.codes $jobs > $dog
        echo ========================================== >> $dog
        for j in $jobs
        do
          echo \
          $d2m4exec $ov -e $expn -s $exp.codes $j >> $dog
          echo ========================================== >> $dog
          $d2m4exec $ov -e $expn -s $exp.codes $j >> $dog 2>&1 || {
            echo difx2mark4 failed on $j; exit 4; }
        done
        $verb && echo -n disk usage on ${expn}: && du -sh $expn
    }
    ;;
4fit-prep)
    $verb && echo prepping fourfit for $expn in `pwd` with $ffconf
    [ -d $exp-$vers-$subv-$label.$expn.save ] && {
        cdata=$exp-$vers-$subv-$label.$expn.save 
    } || {
        echo No correlated data found for fourfit ; exit 4
    }
    echo Using $cdata dir for correlated data
    [ "$workdir" = `pwd` ] || { echo not in working dir ; exit 4 ; }
    [ -d $expn ] && {
        echo The directory $expn already exists, which is incorrect
        echo at this point.  Remove it to continue.
        exit 4
    }
    $dry && { echo dry mode not implemented for this case ; exit 4 ; }
    mkdir $expn
    cp -p $ffconf $expn
    for sd in $cdata/*
    do
      [ -d $sd ] && scan=`basename $sd` && mkdir $expn/$scan || continue
      cd $expn/$scan
      ln -s ../../$cdata/$scan/* .
      cd $workdir
    done
    # make an ovex file and capture the config file
    cd $workdir/$expn
    hops_vex2ovex.py -c ../$exp.codes ../$exp-$vers-$subv.vex.obs $expn.ovex
    cd $workdir
    [ -n "$ehtc" ] && pre=\$ehtc/ || pre=''
    cat <<....EOF
    HOPS directory $expn has been prepared for fourfitting.
    If you haven't worked out manual pc phases:

    pushd $expn
    ${pre}est_manual_phases.py -c $ffconf -r <scan/source.timestamp>
    cp -p $ffconf ..
    popd

    before running a 2nd time with tar=4fit (which does the fitting).
    This fourfit job is labelled with flab '$flab'.
....EOF
    ;;
4fit-4fit)
    $verb && echo running fourfit for $expn in `pwd` with $ffconf
    [ -s "$ffconf" ] || { echo nothing in $ffconf ; exit 4; }
    [ -d $workdir/$expn ] || {
        echo The directory $expn does not exist which makes no
        echo sense at this point.  Review your processing and clean up.
        exit 4
    }
    $dry && { echo dry mode not implemented for this case ; exit 4 ; }
    cd $workdir/$expn
    [ -s "$ffconf" ] || { echo nothing in $expn/$ffconf ; exit 4; }
    set -- `ls */$target*`
    rm -f ff-$label.log
    [ $# -ge 1 ] || { echo no root files to correlate with ; exit 4; }
    echo Launching fourfits on $# scans in parallel...be patient.
    echo Fourfitting version is flab, "'$flab'".
    for r in */$target* ; do fourfit -c $ffconf $r 2>ff-$label.log & done
    wait
    echo Making alist
    alist -v6 -o $exp-$vers-$subv-$label.alist * \
        > $exp-$vers-$subv-$label.alist.warnings 2>&1
    ls -l $exp-$vers-$subv-$label.alist
    echo Removing symlinks prior to tarballing
    cd $workdir
    find $expn -type l -exec rm {} \;
    savename=$exp-$vers-$subv-$label-4fit$flab.$expn.save
    echo savename is $savename
    ;;
esac

# for the tarball cases, actually make the tarball
$dotar && [ -n "$tarname" -a -n "$content" ] && (
    $verb && echo Making $tarname, see $workdir/logs/$tarname.log
    rm -f $workdir/$tarname $workdir/logs/$tarname.log
    exec > $workdir/logs/$tarname.log 2>&1
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
    [ -f $destdir/$tarname ] && {
        echo destination tarball $destdir/$tarname exists; exit 6; }
    echo -n cp -p $workdir/$tarname $destdir ... &&
            cp -p $workdir/$tarname $destdir && echo ok
    [ -f $destdir/$tarname ] && ls -lh $destdir/$tarname
    $nuke && [ -f $destdir/$tarname ] && rm -f $workdir/$tarname
}

# misc cleanup
[ -n "$delete" ] && rm -f $delete

$save && [ -n "$savename" ] && case $work in
fits)
    [ -d $savename ] && mv $savename $savename.prev
    [ -d $fits ] && mv $fits $savename
    ;;
hops|hmix)
    [ -d $savename ] && mv $savename $savename.prev
    [ -d $expn ] && mv $expn $savename
    ;;
4fit-4fit)
    [ -d $savename ] && mv $savename $savename.prev
    [ -d $expn ] && mv $expn $savename
    ;;
esac

exit 0
#
# eof
#
