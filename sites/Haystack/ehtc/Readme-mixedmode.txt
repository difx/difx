#
### Instructions for polconversion/packaging for <Observation>/<Band>
#
# This version is appropriate to mixed-mode packaging.
# The template for this file is in:     $ehtc/Readme-mixedmode.txt
# rename it to:  $exp-$subv-v${vers}${ctry}${stry}p${iter}r${relv}.logfile
#
# Make a copy for each band/track and customize the environment
# variables at the top of the ENVIRONMENT section.
#  v${vers}${ctry}  refers to the DiFX correlation output used
#  ${stry}          refers to the sub-band (b1..4) correlation try
#  p${iter}         refers to the polconvert iteration
#  r${relv}         refers to the release name
#
# For tracking purposes, make a link in the correlation area to this new
# packaging directory so that work can be captured in a few areas.
#
# This version of the PolConvert README generates a fourfit control file,
# but does not estimate the pcphases.
# 
# Cut and paste from this file (which is necessary to get started).
# Once the fourfit control file is in hand, you can execute parts of
# the file using true && { ... } or false && { ... } for partial runs.
#
# (lines with 3 hashes mark places where you should pay attention
#  and/or paste things into this logfile.  The idea is then when done,
#  the $comment file--see below--should produce a useful summary.)
#
# Post copies of this file and all grinding logs to $release/logs.
#

# ENVIRONMENT =====================
# setup versioned tools -- these may be very different per site

# script that adds CASA 4.7.2 bin to PATH
source ~/lib/casa.setup

# source an install of the tagged DiFx 2.9.0a branch
# the difx2fits in this branch has the necessary '--relabelCircular' option for mixed-mode
# and difx2mark4 has 'NVRMAX 98304000' for the 900-sec scans in 2022 and later
source /swc/difx/DiFX-2.9.0-root-25Jul09/setup.bash

# Source a copy of HOPS to get fourfit (not strictly necessary for mixed-mode packaging)
source /swc/hops/x86_64-3.24/bin/hops.bash

# site vars: script area, correlator work dir and release directory

# these point to the mirror location to put the tarballs for release
export hays=/data-sc34/EHT_ARCHIVE/Hays_Output8
export bonn=/data-sc34/EHT_ARCHIVE/Bonn_Output8

# location of packaging scripts
# point to (do not source) a copy of DiFx that has the recent mixed-mode edits to the packaging scripts
# (eg dev branch after April 2026)
export dsvn=/swc/difx/difx-dev/
export ehtc=$dsvn/sites/Haystack/ehtc

# CHECK: indicate site
export arch=$hays

# CHECK: indicate path to correlator output
export corr=/data-sc05/difxoper

# EDIT: indicate path to working folder to build tarballs
export work=/data-sk??/difxoper


# principal vars for tracking all the revisions and forth
export exp=e23...
export vers=?       # major correlator top-level version
export ctry=''      # minor correlator top-level version, a,b,...
export subv=b?      # b1 b2 b3 b4
export stry=''      # minor correlator sub-band version, a1,a2,...
export iter=?       # polconvert iteration, 1,2,...
export relv=?       # archive release name number, 0,1,...
export flab=''      # re-fourfitting version (if needed)
export expn=3...    # HOPS exp # (from Mike Titus)

# $dpfu is estimated from QA2 products, see one-time setup below
# it must be coordinated with the other correlator
export dpfu=0.0308574   # band6
#export dpfu=0.0404810   # band7 ?

# with NOEMA there is some confusion for PolConvert
#export spw=$((${subv/b/} - 1))

# a list of stations in best order for polconvert plots
#export scmp='PV,MG,SW,AX,LM,SZ,GL,MM'
#export scmp='MM,PV,MG,SZ,GL,AX,SW,LM,KT,NN'
#export scmp='KT,MM,PV,MG,SZ,GL,AX,SW,LM,NN'
# number of parallel grinds to schedule (< number physical cores)
#export npar=15
# number of polconvert fringe plots to make
#export npcf=4

#export opts="-r -P $npar -S $scmp -f $npcf -A $dpfu -q $qpar -s $spw"
export opts="-r"
export pcal=$exp
#export plst=$pcal # or "list of all pcal labels"
#export pdir=$hays/$exp/$exp-$vers/qa2
#export ptar=$plab.APP_DELIVERABLES.tgz

# see the tarball script for what this does, should be false
export fitsname=false
# If $work contains multiple jobs or unprocessed jobs, set this to true,
# which implicitly sets the -u flag on any use of $ehtc/ehtc-joblist.py.
# If $work is more messed up than that, you're on your own for coping.
export uniq=true    # or export uniq=false
# if you want to preserve the original *difx -> *save behavior
# export keepdifxout=True

# other derived vars
export release=$arch/$exp/$exp-$relv
export dout=$corr/$exp/v${vers}$ctry/$subv$stry
export evs=$exp-$vers-$subv
export ers=$exp-$relv-$subv
#export aeditjob=$ehtc/ehtc-aeditjob.sh

echo
echo 'DiFX data is in: ' $dout
echo 'Packaging will happen in: ' $work/$exp/v${vers}${ctry}p${iter}/$subv
echo 'Release will go to: ' $release
echo


# check:
wordcount=`(
echo =============================================================== && \
echo $exp $vers .$ctry. $subv .$stry. $iter $relv .$flab. $expn && \
echo $evs $ers $opts $pcal && \
echo $fitsname && \
echo $dout && \
echo $release && \
echo =============================================================== && \
type difx2fits && \
type difx2mark4 && \
echo =============================================================== 
) | wc -w`

#echo $wordcount
[ "$wordcount" -eq 25 ] && echo variables are ok || { echo PROBLEM WITH VARIABLES!!! ; }


# Nth TIME SETUP =================
cd $work/$exp/v${vers}${ctry}p${iter}/$subv

# ONE TIME SETUP =================
false && { # ONE TIME SETUP
[ -d $work/$exp/v${vers}${ctry}p${iter} ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}
[ -d $work/$exp/v${vers}${ctry}p${iter}/$subv ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}/$subv
[ -d $release ] || mkdir -p $release
pwd ; cd $work/$exp/v${vers}${ctry}p${iter}/$subv ; pwd
ls -l $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile
obsband=`grep '^### Instructions' *.logfile | sed 's/.*for[ ]*//'`
[ "$obsband" = $exp/$subv ] || {
    echo title of file is "$obsband" not $exp/$subv ; }

# once per trak, not per band, set up for polconvert data
# from the mirror after consultation with the other correlator
# $hays is used here on the assumption that the tarballs appear there
# first but it could as well be $bonn if it starts there.  Either way
# both correlators should unpack the same QA2 tarballs.
[ -d $hays/$exp ] || mkdir $hays/$exp
[ -d $hays/$exp/$exp-$relv ] || mkdir $hays/$exp/$exp-$relv


# pull in the experiment codes
cp -p $ehtc/ehtc-template.codes $exp.codes
cp -p $dout/*vex.obs $dout/*.v2d .
[ `ls -l *vex.obs | wc -l` -eq 1 ] || echo Too many/too few vex.obs files

# haxp is generated in $dout so preserve $expn if found:
[ -d $dout/$expn ] && mv $dout/$expn $dout/$expn.save

# ehtc-tarballs.sh haxp expects *.codes in $dout otherwise it fails silently
[ -f $dout/$exp.codes ] && cmp $ehtc/ehtc-template.codes $dout/$exp.codes ||
cp -p $ehtc/ehtc-template.codes $dout/$exp.codes

# clean slate fourfit control file
cat > $ers.bare <<EOF
* bare fourfit config file for ALMA 1mm session
weak_channel 0.0
optimize_closure true
pc_mode manual
mbd_anchor sbd
gen_cf_record true
sb_win -1.024   1.024
mb_win -0.008    0.008
dr_win -0.00001 0.00001
* adjustments follow
EOF

# outputband frequency labels
# uncomment ONE set of chan_ids appropriate to the band, comment out the others:

# 230GHz
#$ehtc/alma-vex-defs.py -rchan -f213100.0 -sL -w58.0 >> $ers.bare  # b1
#$ehtc/alma-vex-defs.py -rchan -f215100.0 -sL -w58.0 >> $ers.bare  # b2
#$ehtc/alma-vex-defs.py -rchan -f215091.0 -sL -w58.0 >> $ers.bare  # b2 SiO e24f11
#$ehtc/alma-vex-defs.py -rchan -f227100.0 -sU -w58.0 >> $ers.bare  # b3
#$ehtc/alma-vex-defs.py -rchan -f229100.0 -sU -w58.0 >> $ers.bare  # b4

# 260GHz for e24b04 - misaligned to expected, due to ALMA mistuning
#$ehtc/alma-vex-defs.py -rchan -f251531.25 -sL -w58.0    >> $ers.bare  # b1
#$ehtc/alma-vex-defs.py -rchan -f253589.84375 -sL -w58.0 >> $ers.bare  # b2
#$ehtc/alma-vex-defs.py -rchan -f265531.25 -sU -w58.0    >> $ers.bare  # b3
#$ehtc/alma-vex-defs.py -rchan -f267589.84375 -sU -w58.0 >> $ers.bare  # b4

# 345GHz - varies by year - CHECK that these frequencies agree with the actual outputbands
#$ehtc/alma-vex-defs.py -rchan -f335621.00 -sL -w58.0     >> $ers.bare  # b1 2023
#$ehtc/alma-vex-defs.py -rchan -f335600.00 -sL -w58.0     >> $ers.bare  # b1 2024
#$ehtc/alma-vex-defs.py -rchan -f337516.50 -sL -w58.0     >> $ers.bare  # b2 2023
#$ehtc/alma-vex-defs.py -rchan -f337544.25625 -sL -w58.0  >> $ers.bare  # b2 2024
#$ehtc/alma-vex-defs.py -rchan -f347621.0 -sU -w58.0      >> $ers.bare  # b3 2023
#$ehtc/alma-vex-defs.py -rchan -f347600.0 -sL -w58.0      >> $ers.bare  # b3 2024
#$ehtc/alma-vex-defs.py -rchan -f349579.0 -sU -w58.0      >> $ers.bare  # b4 2023
#$ehtc/alma-vex-defs.py -rchan -f349600.0 -sU -w58.0      >> $ers.bare  # b4 2024

# and we are finished with the basic control file for this track/band
cp -p $ers.bare $ers.conf

# on later iterations you can re-use what you have available
# ff_conf=`ls -t $corr/$exp/v${vers}*p*/$evs.conf | head -1`
# ff_conf=`ls -t $work/$exp/v${vers}*p*/$evs.conf | head -1`
# [ -f "$ff_conf" ] && cp -p $ff_conf . && ls -l $evs.conf
# rename to the release version
# mv $evs.conf $ers.conf
ls -l $ers.conf

# maintain notes while you execute and post this file periodically to
[ -d $release/logs ] || mkdir $release/logs
# verify that this file is:
ls -l $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile

# provide a number of summary reports prior to any grinding
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -B > $ers-bl-pol-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -D > $ers-ant-ch-bl-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -F > $ers-ant-ch-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -R > $ers-jobs-map.txt
( cd $dout ; summarizeDifxlogs.py    ) > $ers-difxlog-sum.txt
( cd $dout ; summarizeDifxlogs.py -c ) > $ers-difxlog-clr.txt
cp -p $ers*.txt $release/logs
cp -p $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile $release/logs


### Break here; the rest of the ONE TIME SETUP is optional fringe checks
### which are not required for mixed-mode releases. Do this line-by-line, if you want.
}
false && {

#
# Run the GENERAL PROCESSING commands on a few jobs to make suitable data
# for generating "good enough" manual phase cals (i.e. for survey use)
# Ideally you should use one QA2_proj.
# Make ### notes suitable for later grepping
#
# Notes on building $ers.conf:
# pick the scans to use based on greps from *jobs*
### available stations: ...
awk '{print $5}' $ers-jobs-map.txt | tr '-' \\012 | sort | uniq -c
awk '{print $5}' $ers-jobs-map.txt | tr '-' \\012 | sort | uniq | tr \\012 ' '
### types of baselines: ...
awk '{print $5}' $ers-jobs-map.txt | sort | uniq -c
# and set jobs and target for the fewest scans on strong targets for pcals
target=...
jobs=`echo $exp-$vers-${subv}_{,}.input` ; echo $jobs

# REMINDER: for multi-project tracks you will need to update
# $opts and $pcal using the QA_* logic variables above.
# --override-version  on difx2mark4 should not be necessary
# Remember that prepolconvert will relabel ALMA pols as (X/Y->L/R)
# prepolconvert.py -v -k -s $dout $jobs
# drivepolconvert.py -v $opts -l $pcal $jobs
drivepolconvert.py -v -p -k -D $dout $opts -l $pcal $jobs
for j in $jobs ;\
do difx2mark4 -e $expn -s $exp.codes --override-version ${j/input/difx} ; done

# work in the $expn directory created by difx2mark4
cd $expn ; cp ../$ers.conf . ; cp -p ../$ers.bare .

# identify roots:
roots=`ls */$target*` ; echo $roots

# if you are not sure about which scans to calibrate with which stations...
for r in $roots; do fourfit -pt -c $ers.bare -b A? $r ; done
# if you need to add more scans, make sure (after polconversion) that the
# jobs variable reflects ALL scans (to be deleted at the end).

# For each root run est_manual_phases.py, but set -s argument
# with a different comma-sep list of single letter station codes
# that should be fit (relative to A as first station).
# Refer to the station codes file for the 2-letter to 1-letter codes.
$ehtc/est_manual_phases.py -c $ers.conf -v \
    -r first-root -s A,x,y,z,...
grep ^if.station $ers.conf | sort | uniq -c
$ehtc/est_manual_phases.py -c $ers.conf -v \
    -r second-root -s A,p,q,r,...
grep ^if.station $ers.conf | sort | uniq -c
# ... iterate with additional roots as you find you need them
# you should note whether all steps were completed; this is not
# necessarily a problem, but you may want to choose better scans.
#
# The -v option turns on some progress so that you monitor progress.
# It will declare some steps not done if full convergence is not
# reached...this is generally not a problem.
#...
### are all manual phases set up plausibly?  tell us what you think.
for r in $roots ; do fourfit -bA? -c $ers.conf $r & done
# wait
fplot */A[^A].B*
### first-root
### SNR  LL   RR   LR   RL
### ...
### second-root
### SNR  LL   RR   LR   RL
### ...

#
# Reset jobs so that it reflects all the data brought in by prepolconvert:
# now clean up to restore a clean directory, especially to move $expn aside
cd ..
jobs=`ls $exp-$vers-${subv}_*.input` ; echo $jobs
cp -p $expn/$ers.conf .
cp -p $expn/$ers.conf $release/logs
mv $expn ff-conf-$expn
rm -rf ${jobs//input/*}
# this should be a short list (i.e. no DiFX job files):
ls -latr | grep -v .ms. | grep -v README | grep -v qa2
# and now we have $ers.conf for the grind below

} # ONE TIME SETUP

#--------------------------------------------------------------------------
# IF YOU NEED TO RE-DO anything, either start over in a new $iter or make
# this directory pristine for a block with a suitable replacement for ????
false && {
  rm -rf $exp-$vers-${subv}_????.{calc,difx,flag,input,save,im,polc*}
  rm -rf tb-*
}
#--------------------------------------------------------------------------

# EXECUTION NOTES ======================
# Capture all commands in this file.
# Use true && { ... } for blocks of commands to be executed as a group.
# then change true to false to prevent re-execution on the next group.
# As needed insert comments (for grepping later):
#   # ... details of interest to correlator folks
#   ###  important messages that can be grepped out to make a summary
#
# TODO list ======================
# This command (without false) generates blocks of commands to insert here:
false &&
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -L -Y
# The '-Y' flag indicates the mixed-mode option for the ehtc-jsgrind syntax

###
### log of $ers commands goes here
###
### make absolutely sure that all grinds are contained within
### $QA... && { ... } so that the file can be sourced to set vars.
###

#
# Once you have edited this file to run a block of grinding jobs with correct
# true/false controls, you can launch with this (only one matching logfile):
#
# sh *.logfile & disown
#
#--------------------------------------------------------------------------
# Final Steps ======================
# The manual checks in the typical polconvert logfile are not possible here,
# without any polconversion or 4fit'ing.
# No antabs or aedit plots are generated as part of the	mixed-mode release.
#
false && {
# save logfile incrementally or when done:
cp -p $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile $release/logs
ls -l $release/logs


# when ready to release, check on space in Archive:
du -sBG tb-* | awk '{t+=$1}END{print t "G"}'
df -h $release
# if there is more than enough space: execute in parallel:
for r in tb-* ; do pushd $r ; nohup ./release.sh & disown ; popd ; done
# if you aren't sure, execute them serially
for r in tb-* ; do pushd $r ; nohup ./release.sh & wait   ; popd ; done &
# monitor (if you want) with
du -scBG tb-*

# and finally after everything is released count the products
# this script requires polconvert inputs like $plst
# should try to make it work for mixed-mode packaging
#$ehtc/ehtc-release-check.sh | sed 's/^/### /'

# after tarballs are delivered you can remove the polconvert swin dirs:
# (these are not produced in the mixed-mode packaging; the .save folders are in $dout)
#rm -rf $exp-$vers-${subv}_????.{save,difx}

# one last time
logfile=$exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile
comment=$exp-$subv-v${vers}${ctry}p${iter}r${relv}.comment
grep '^###' $logfile > $comment
cp -p $logfile $comment $release/logs
ls -l $release/logs

# Cleanup list ======================
# the product dirs should probably be saved until the archive is final...
#ls -ld $exp-*-${subv}*.save
# ...but you can remove them too:
#rm -rf $exp-*-${subv}*.save
#rm -rf $dout/$exp-*-${subv}*.save
}
# avoid worrisome error return values
true

#
# eof vim: nospell noai
#
