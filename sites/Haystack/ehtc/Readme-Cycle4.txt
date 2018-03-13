#
# Canned instructions for polconversion and packaging for ...
#
# This version is appropriate to Cycle4 following Rev3
# The template for this file is in:     $ehtc/Readme-Cycle4.txt
# rename it to:      $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile
#
# Make a copy for each band/track and customize the environment
# variables at the top of the ENVIRONMENT section.
#

# ENVIRONMENT =====================
# go to working host and setup versioned tools -- different per site
to eht
casadev   # alias that adds CASA 4.7.2 bin to PATH
source /swc/difx/setup-DiFX-2.5.2.bash
#source /swc/difx/setup-DiFX-2.5.bash
#source /swc/difx/setup-difx.bash
#source /swc/difx/difx-root-18Jan09/setup-difx.bash
#source /swc/hops/hops.bash
source $DIFXROOT/bin/hops.bash

# site vars: these point to the mirror or difx SVN tree
export hays=/data-sc04/EHT_ARCHIVE/Hays_Output
export bonn=/data-sc04/EHT_ARCHIVE/Bonn_Output
export dsvn=/swc/difx/difx-svn
# site vars: script area, correlator work dir and release directory
export ehtc=$dsvn/sites/Haystack/ehtc
# export ehtc=/swc/scripts/ehtc
export arch=$hays
export corr=/data-sc05/difxoper
export work=/data-sc15/difxoper

# principal vars for tracking all the revisions and forth
export exp=e17...
export vers=3
export ctry=''
export subv=lo
export iter=5
export relv=4
export flab=''
export expn=3...

# polconvert variables and other option variables
export pcal=TRACK_?
export pdir=$hays/$exp/$exp-$vers/qa2
export dpfu=0.0308574
export scmp='PV,AZ,SM,AP,LM,SR,SP'
export opts="-r -P15 -S $scmp -f 4 -A $dpfu"
export fitsname=false

# derived vars
export release=$arch/$exp/$exp-$relv
export dout=$corr/$exp/v${vers}$ctry
export evs=$exp-$vers-$subv
export ers=$exp-$relv-$subv
export ptar=$pcal.APP_DELIVERABLES.tgz

# check:
[ `( 
echo =============================================================== && \
echo $exp $vers .$ctry. $subv $iter $relv .$flab. $expn && \
echo $evs $ers $opts $fitsname && \
echo $dout && \
echo $ptar $pcal && \
echo $pdir && \
echo $release && \
echo =============================================================== && \
type casa && \ 
type mpifxcorr && \
type fourfit && \
echo =============================================================== 
) | wc -w` -eq 36 ] && echo variables are ok || echo issue with variables

# Nth TIME SETUP =================
cd $work/$exp/v${vers}${ctry}p${iter}

# ONE TIME SETUP =================
[ -d $work/$exp/v${vers}${ctry}p${iter} ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}
[ -d $release ] || mkdir $release
cd $work/$exp/v${vers}${ctry}p${iter}
tar zxf $pdir/$ptar

ls -ld $pcal.*
cp -p $ehtc/ehtc-cycle4.codes $exp.codes
cp -p $dout/*vex.obs .

# haxp is generated in $dout so preserve $expn if found:
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
# may need to edit the above or just use one of these
ff_conf=`ls -t $corr/$exp/v${vers}*p*/$evs.conf | head -1`
ff_conf=`ls -t $work/$exp/v${vers}*p*/$evs.conf | head -1`
[ -f "$ff_conf" ] && cp -p $ff_conf . && ls -l $evs.conf
# rename to the release version
mv $evs.conf $ers.conf

# generate TODO list with:
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -G

# maintain notes while you execute and post this file periodically to
[ -d $release/logs ] || mkdir $release/logs
# verify that this file is:
# $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile

# GENERAL PROCESSING TEMPLATE ======================
export proj=yyy targ=XXX class=cal|sci
export label=$proj-$targ
export jselect="-p $proj -s $targ"
eval `$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -J`
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -R
# review list of jobs and paste into logfile, review $scmp, $opts
# pull over data and evaluate partitioning of job list
prepolconvert.py -v -k -s $dout $jobs
$ehtc/ehtc-zoomchk.py -v $jobs
# subdivide $jobs as instructed and do the polconvert on each set of $jobs
drivepolconvert.py -v $opts -l $pcal $jobs
# evaluate results on full set of $jobs--look at ALL_IFs plots; then
#--------------------------------------------------------------------------
# A: until you have the fourfit control file built:
$ehtc/ehtc-postdrive.sh echo false $jobs
$ehtc/ehtc-postdrive.sh eval false $jobs
# ie.: cd $expn ; $ehtc/est_manual_phases.py -c $evs.conf <root> ; cd ..
# B: thereafter, where you just re-use it.
$ehtc/ehtc-postdrive.sh echo $jobs
$ehtc/ehtc-postdrive.sh eval $jobs
#--------------------------------------------------------------------------
# archive to output (after reviewing/checking some of the products)
ls -lh ./tarballs/*
[ -d $release/$proj-$class ] || mkdir $release/$proj-$class
mv ./tarballs/* $release/$proj-$class
ls -lh $release/$proj-$class/$ers-$proj-$targ*tar
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
cp -p $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile $release/logs
# check on progress/missing scans (incrementally or when done):
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -c $exp.codes -K |\
  sort -k 1n > $release/logs/$ers-manifest.txt

# TODO list ======================
# $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -G

# Cleanup list ======================
# after tarballs are delivered and you want to recover disk space
rm -rf $exp-$vers-${subv}_*.save
rm -rf $exp-$relv-${subv}_*.save

#
# eof
#
