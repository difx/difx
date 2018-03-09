#
# Canned instructions for polconversion and packaging for Jan DR 2018.
#
# This version is appropriate to Cycle5 onwards
# The template for this file is in:     $ehtc/Readme-Cycle5.txt
# rename it to:      $exp-$subv-v${vers}${ctry}p${iter}.logfile
#
# Make a copy for each band/track and customize the environment
# variables at the top of the ENVIRONMENT section.
#

# ENVIRONMENT =====================
# go to working host and setup tools -- different per site
to eht
casadev   # alias that adds CASA 4.7.2 bin to PATH
#source /swc/difx/setup-DiFX-2.5.bash
source /swc/difx/setup-difx.bash
#source /swc/difx/difx-root-18Jan09/setup-difx.bash
#source /swc/hops/hops.bash
source $DIFXROOT/bin/hops.bash

# site vars: these point to the mirror or difx SVN tree
export hays=/data-sc04/EHT_ARCHIVE/Hays_Output
export bonn=/data-sc04/EHT_ARCHIVE/Bonn_Output
export dsvn=/swc/difx/difx-svn
# site vars: script area, correlator work dir and release directory
#export ehtc=$dsvn/sites/Haystack/ehtc
export ehtc=/swc/scripts/ehtc
export arch=$hays
export corr=/data-sc15/difxoper
export work=/data-sc15/difxoper

# principal vars for tracking all the revisions and forth
export exp=e18j28
export vers=1
export ctry=''
export subv=b1
export iter=1
export relv=1
export flab=''
export expn=3635

# the polconvert variables
export pcal=2017_7CSV
export pdir=$hays/$exp/$exp-$vers/qa2
export dpfu=0.0308574
export scmp='PV,MG,SW,AQ,LM,SR,SZ'
export opts="-r -P15 -S $scmp -f 4 -A $dpfu -q v4"

# derived vars
export release=$arch/$exp/$exp-$relv
export dout=$corr/$exp/v${vers}$ctry/$subv
export evs=$exp-$vers-$subv
export ers=$exp-$relv-$subv
export ptar=${pcal}.APP_DELIVERABLES.tgz

# check:
[ `( 
echo =============================================================== && \
echo $exp $vers .$ctry. $subv $iter $relv .$flab. $expn && \
echo $evs $ers $opts && \
echo $dout && \
echo $ptar $pcal && \
echo $pdir && \
echo $release && \
echo =============================================================== && \
type casa && \ 
type mpifxcorr && \
type fourfit && \
echo =============================================================== 
) | wc -w` -eq 37 ] && echo variables are ok || echo issue with variables

# Nth TIME SETUP =================
cd $work/$exp/v${vers}${ctry}p${iter}/$subv

# ONE TIME SETUP =================
[ -d $work/$exp/v${vers}${ctry}p${iter} ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}
[ -d $work/$exp/v${vers}${ctry}p${iter}/$subv ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}/$subv
[ -d $work/$exp/v${vers}tb ] ||
    mkdir $work/$exp/v${vers}tb
[ -d $release ] || mkdir $release
cd $work/$exp/v${vers}${ctry}p${iter}/$subv

# once per trak, not per band:
[ -d $hays/$exp ] || mkdir $hays/$exp
[ -d $hays/$exp/$exp-$vers ] || mkdir $hays/$exp/$exp-$vers
[ -d $hays/$exp/$exp-$vers/qa2 ] || mkdir $hays/$exp/$exp-$vers/qa2
# use a common qa2 dir for multiple bands
# if there are multiple versions of the tarball you will
# need to pick the appropriate one
[ -d ../qa2 ] || { mkdir ../qa2 ; pushd ../qa2 ; tar zxf $pdir/$ptar ; popd ; }

# link in QA2 for this band
for d in ../qa2/$pcal.* ; do ln -s $d . ; done

cp -p $ehtc/ehtc-template.codes $exp.codes
cp -p $dout/*vex.obs .

# if haxp is to be generated -- this is the default:
cp -p $exp.codes $dout
# and it works in $dout so save this or bad things may happen
ls -ld $dout/$expn
[ -d $dout/$expn ] && mv $dout/$expn $dout/$expn.save

# clean slate fourfit control file
cat > $evs.conf <<EOF
* fourfit config file for ALMA 1mm session April 2017 $evs
weak_channel 0.0
optimize_closure true
pc_mode manual
mbd_anchor sbd
gen_cf_record true
sb_win -0.10     0.10
mb_win -0.008    0.008
dr_win -0.000001 0.000001
* adjustments follow
EOF
# may need to edit the above or just use one of these, e.g.
ff_conf=`ls -t $corr/$exp/v${vers}*p*/$evs.conf | head -1`
ff_conf=`ls -t $work/$exp/v${vers}*p*/$evs.conf | head -1`
[ -f "$ff_conf" ] && cp -p $ff_conf . && ls -l $evs.conf
mv $evs.conf $ers.conf

# generate TODO list with:
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -G

# maintain notes while you execute and post this file periodically to
[ -d $release/logs ] || mkdir $release/logs
# verify that this file is:
# $exp-$subv-v${vers}${ctry}p${iter}.logfile

# GENERAL PROCESSING TEMPLATE ======================
export proj=yyy targ=XXX class=cal|sci
export label=$proj-$targ
export jselect="-p $proj -s $targ"
eval `$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -J`
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -R
# review list of jobs, review $scmp, $opts
prepolconvert.py -v -k -s $dout $jobs
# evaluate partitioning of job list (use -v if error)
$ehtc/ehtc-zoomchk.py -v $jobs
# subdivide $jobs as necessary, do the polconvert on each set of $jobs
drivepolconvert.py -v $opts -l $pcal $jobs
# evaluate results on full set of $jobs--look at ALL_IFs plots; then
#--------------------------------------------------------------------------
# A: until you have the fourfit control file built:
# cd $expn ; $ehtc/est_manual_phases.py -c $evs.conf <root> ; cd ..
$ehtc/ehtc-postdrive.sh echo false $jobs
$ehtc/ehtc-postdrive.sh eval false $jobs
# B: thereafter, where you just re-use it.
$ehtc/ehtc-postdrive.sh echo $jobs
$ehtc/ehtc-postdrive.sh eval $jobs
# C: any other products  (e.g. other FITS, as TBD; haxp is now done above)
#--------------------------------------------------------------------------
# archive to output (after reviewing/checking some of the products)
ls -lh ./tarballs/*
[ -d $release/$proj-$class ] || mkdir $release/$proj-$class
mv ./tarballs/* $release/$proj-$class
ls -lh $release/$proj-$class/$evs-$proj-$targ*tar
# send email to GBC if tarballs are to be released outside of Corr WG

# EXECUTION NOTES ======================

# paste all commands as at the shell prompt
# pause to observe correct progress and make comments:
#   # ... details of interest to correlator folks
#   ###  important messages that can be grepped out to make a summary
#--------------------------------------------------------------------------

<<<log of commands goes here>>>

#--------------------------------------------------------------------------
# save logfile incrementally or when done:
cp -p $exp-$subv-v${vers}${ctry}p${iter}.logfile $release/logs
# check on progress/missing scans:
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -c $exp.codes -K

# TODO list ======================
# $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -G

# Cleanup list ======================
# after tarballs are delivered and you want to recover disk space
rm -rf $exp-$vers-${subv}_*.save

#
# eof
#
