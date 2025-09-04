#
### Instructions for polconversion/packaging for <Observation>/<Band>
#
# This version is appropriate to Cycle9 (b1..b4) (2023 and later).
# It differs only in minor ways from the Cycle8 version (2021 and later).
# The template for this file is in:     $ehtc/Readme-Cycle9.txt
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
# polconvert directory so that work can be captured in a few areas.
#
# If there are MULTIPLE PROJECTS with SEPARATE QA2 DELIVERABLES for
# each, you will need to manage the sets of QA2 calibrations using
# QA2_proj logic variables.  I.e. you will have 2 or more passes of
# setup and grinding using this file.  Final release is still by track.
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
# setup versioned tools -- very different per site
# script that adds CASA 4.7.2 bin to PATH
source ~/lib/casa.setup
#source /swc/difx/setup-DiFX-2.7.1.bash     # 2021 production
#source /swc/difx/setup-DiFX-2.7.bash       # 2021 test
#source /swc/difx/setup-DiFX-2.6.2.bash     # 2017,2018
#source /swc/difx/setup-DiFX-2.6.3.bash     # testing
#source /swc/difx/setup-DiFX-2.7.bash       # 2021 target
#source /swc/difx/setup-difx.bash           # testing
#source /swc/hops/hops.bash
source /cluster/difx/DiFX-281/setup_difx    # 2022,2023 production - with patch for subarrayed tracks https://github.com/difx/difx/pull/67
# Only if you had somehow previously set it up:
# export HOPS_SETUP=false
source $DIFXROOT/bin/hops.bash

# site vars: these point to the mirror or difx SVN tree
export hays=/data-sc25/EHT_ARCHIVE/Hays_Output3
export bonn=/data-sc25/EHT_ARCHIVE/Bonn_Output3
#xport dsvn=/swc/difx/difx-svn
#export dsvn=/swc/difx/difx-svn/master_tags/DiFX-2.6.2
#export dsvn=/swc/difx/difx-svn/master_tags/DiFX-2.7
#export dsvn=/swc/difx/difx-svn/master_tags/DiFX-2.7.1
export dsvn=/swc/difx/difx-git/master_tags/DiFX-2.8.1
# site vars: script area, correlator work dir and release directory
#export ehtc=$dsvn/sites/Haystack/ehtc
#export ehtc=/swc/scripts/ehtc
export ehtc=$dsvn/sites/Haystack/ehtc
export arch=$hays
export corr=/data-sc15/difxoper
# run polconvert on the same machine with the files
export work=/data-sc15/difxoper
export work=/data-sc14/gbc

# principal vars for tracking all the revisions and forth
export exp=e23...
export vers=?       # major correlator top-level version
export ctry=''      # minor correlator top-level version, a,b,...
export subv=b?      # b1 b2 b3 b4
export stry=''      # minor correlator sub-band version, a,b,...
export iter=?       # polconvert iteration, 1,2,...
export relv=?       # archive release name number, 0,1,...
export flab=''      # re-fourfitting version (if needed)
export expn=3...    # HOPS exp # (from Mike Titus)

# $dpfu is estimated from QA2 products, see one-time setup below
# it must be coordinated with the other correlator
export dpfu=0.0308574   # band6
#xport dpfu=0.0404810   # band7 ?

# with NOEMA there is some confusion for PolConvert
export spw=$((${subv/b/} - 1))

# a list of stations in best order for polconvert plots
#export scmp='PV,MG,SW,AX,LM,SZ,GL,MM'
#export scmp='MM,PV,MG,SZ,GL,AX,SW,LM,KT,NN'
export scmp='KT,MM,PV,MG,SZ,GL,AX,SW,LM,NN'
# number of parallel grinds to schedule (< number physical cores)
export npar=15
# number of polconvert fringe plots to make
export npcf=4

# define one logic variable QA2_proj for each project nickname <proj>
# in the track and set plab, pcal and qpar appropriately for it.  You
# will need to repeat part of the one-time setup for each QA2 setup.
# make sure that precisely one QA2_proj variable can be true
#
# note that non-ALMA time still needs pcal and qpar defined
# (due to the script checking; but the values are not used).
#
# If there is no ALMA in the track, there will be no polconversion
# and you need to pay attention as some of the setup does not happen.
# If drivepolconvert -l does not respect the 'none' label, you will
# need to create the tables it expects to see.
#
# if there is only one ALMA track, you may set that QA2_proj variable
# as true for the initial setup (unpacking DELIVERABLES, &c)... and even
# comment out some of the checking logic; but once you create the
# the grinding jobs (...ehtc-joblist ... -L below), you must be careful
# not to source this file unless you plan to grind things.  For setting
# plab, pcal and qpar, either copy from a previous iteration or (first time)
# this will be handy:   tar ztvf .../*.APP_DELIVERABLES.tgz &c.
export QA2_proj=false
export QA2_na=false
$QA2_proj && {
    $QA2_na && echo QA2 logic error && exit
    export plab=<track>-<exp>-<yyyymmdd>           # external QA2 file label
    export pcal=???????                            # internal QA2 label
    export qpar=v?                                 # README.DRIVEPOLCONVERT
}
$QA2_na && echo QA2 logic error && exit
export opts="-r -P $npar -S $scmp -f $npcf -A $dpfu -q $qpar -s $spw"
export plst=$pcal # or "list of all pcal labels"
export pdir=$hays/$exp/$exp-$vers/qa2
export ptar=$plab.APP_DELIVERABLES.tgz

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
export aeditjob=$ehtc/ehtc-aeditjob.sh

# check:
wordcount=`(
echo =============================================================== && \
echo $exp $vers .$ctry. $subv .$stry. $iter $relv .$flab. $expn && \
echo $evs $ers $opts && \
echo $fitsname $aeditjob && \
echo $dout && \
echo $ptar $pcal $qpar && \
echo $pdir $npar $scmp $npcf $dpfu && \
echo $release && \
echo =============================================================== && \
type casa && \
type mpifxcorr && \
type fourfit && \
echo =============================================================== 
) | wc -w`
[ "$wordcount" -eq 48 ] && echo variables are ok || {
    [ "$wordcount" -eq 45 ] && echo variables ok, but QA2 all false ||
    { echo issue with variables ; exit ; }
}

# Nth TIME SETUP =================
cd $work/$exp/v${vers}${ctry}p${iter}/$subv${stry}
ls -l $exp-$subv-v${vers}${ctry}${stry}p${iter}r${relv}.logfile

# ONE TIME SETUP =================
false && { # ONE TIME SETUP
[ -d $work/$exp/v${vers}${ctry}p${iter} ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}
[ -d $work/$exp/v${vers}${ctry}p${iter}/$subv${stry} ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}/$subv${stry}
[ -d $release ] || mkdir -p $release
pwd ; cd $work/$exp/v${vers}${ctry}p${iter}/$subv${stry} ; pwd
ls -l $exp-$subv-v${vers}${ctry}${stry}p${iter}r${relv}.logfile
obsband=`grep '^### Instructions' *.logfile | sed 's/.*for[ ]*//'`
[ "$obsband" = $exp/$subv ] || {
    echo title of file is "$obsband" not $exp/$subv ; }

# once per trak, not per band, set up for polconvert data
# from the mirror after consultation with the other correlator
# $hays is used here on the assumption that the tarballs appear there
# first but it could as well be $bonn if it starts there.  Either way
# both correlators should unpack the same QA2 tarballs.
[ -d $hays/$exp ] || mkdir $hays/$exp
[ -d $hays/$exp/$exp-$vers ] || mkdir $hays/$exp/$exp-$vers
[ -d $hays/$exp/$exp-$vers/qa2 ] || mkdir $hays/$exp/$exp-$vers/qa2

# locally, use a common qa2 dir for multiple bands
[ -d $pdir ] || { mkdir -p $pdir && echo need DELIVERABLES tarball in $pdir ; }
[ -d ../qa2 ] || { mkdir ../qa2 ; }
# for every QA2_proj, define $pcal,$pdir,$ptar and untar the $pdir/$ptar files
for pc in $plst ; do [ -d ../qa2 -a -d ../qa2/$pc.qa2-diagnostics ] ||
    echo $pc tables are missing, you need to untar them. ; done
# source the setup for each QA2_proj and then do this snippet:
[ -d ../qa2 -a -d ../qa2/$pcal.qa2-diagnostics ] ||
    { pushd ../qa2 ; tar zxf $pdir/$ptar ;
      for r in README* ; do mv $r $pcal.$r ; done ; popd ; }

# for every QA2_proj, link in the QA2 package tables
for pc in $plst; do
for d in ../qa2/$pc.* ; do ln -s $d . ; done; ls -ld $pc.*
done

# Review README.DRIVEPOLCONVERT: it may specify something other than 'v8'
# which is the value passed as the -q argument to drivepolconvert.py
# via the opts='...' assignment above.  Make sure you have this right.
for pc in $plst; do
echo -n 'CHECK: ' $opts '== ' ; cat $pc.README.DRIVEPOLCONVERT
done

# for every QA2 package, estimate a common $dpfu for the campaign
# dpfu (degrees / flux unit) appears in the ANTAB files and it is a
# somewhat arbitrary choice.  $pcal.APP.artifacts is a link to find tables
for pc in $plst; do
  rm -f *.APP.artifacts
  ln -s $pc.qa2-diagnostics $pc.APP.artifacts
  ls -l $pc.APP.artifacts
  casa --nologger --nologfile -c $DIFXROOT/share/polconvert/DPFU_scanner.py
done 2>&1 | tee dpfu-scanner.log
# when you are done with this remove it as, DPFU_scanner expects only one
rm -f *.APP.artifacts

# Note and paste values here for each track, and after discussion
# with the other correlator, update the dpfu= assignment above.
grep Average dpfu-scanner.log | sed 's/^/### /'

# pull in the experiment codes: note J is single-pol 2018 and previous
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
*sb_win -1.024   1.024
sb_win -0.10     0.10
mb_win -0.008    0.008
dr_win -0.000001 0.000001
* adjustments follow
EOF
# add chan_ids directive appropriate to the band, comment out the others:
# band6
#$ehtc/alma-vex-defs.py -rchan -f213100.0 -sL -w58.0 >> $ers.bare  # b1
#$ehtc/alma-vex-defs.py -rchan -f215100.0 -sL -w58.0 >> $ers.bare  # b2
#$ehtc/alma-vex-defs.py -rchan -f227100.0 -sU -w58.0 >> $ers.bare  # b3
#$ehtc/alma-vex-defs.py -rchan -f229100.0 -sU -w58.0 >> $ers.bare  # b4
# band7
#$ehtc/alma-vex-defs.py -rchan -f335621.00 -sL -w58.0     >> $ers.bare  # b1
#$ehtc/alma-vex-defs.py -rchan -f337516.50 -sL -w58.0     >> $ers.bare  # b2
#$ehtc/alma-vex-defs.py -rchan -f347621.00 -sU -w58.0     >> $ers.bare  # b3
#$ehtc/alma-vex-defs.py -rchan -f349579.00 -sU -w58.0     >> $ers.bare  # b4
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
ls -l $exp-$subv-v${vers}${ctry}${stry}p${iter}r${relv}.logfile

# provide a number of summary reports prior to any grinding
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -B > $ers-bl-pol-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -D > $ers-ant-ch-bl-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -F > $ers-ant-ch-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -R > $ers-jobs-map.txt
( cd $dout ; summarizeDifxlogs.py    ) > $ers-difxlog-sum.txt
( cd $dout ; summarizeDifxlogs.py -c ) > $ers-difxlog-clr.txt
cp -p $ers*.txt $release/logs
cp -p $exp-$subv-v${vers}${ctry}${stry}p${iter}r${relv}.logfile $release/logs

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
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -L
#

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
# This section is always MANUAL as you must LOOK at things.
#
false && {
# save logfile incrementally or when done:
cp -p $exp-$subv-v${vers}${ctry}${stry}p${iter}r${relv}.logfile $release/logs
ls -l $release/logs

# capture the performance on polconversion for future reference
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -T | tee $ers-performance.txt
cp -p $ers-performance.txt $release/logs

### Review PolConvert output.
# first, make checks for polconvert issues
grep -l SEVERE *pol*/casa*/*output |\
    cut -d_ -f2 | cut -d. -f1 | tr \\012 ' '
grep -l 'was NOT polconverted properly' *polcon*/casa-logs/*output |\
    cut -d_ -f2 | cut -d. -f1 | tr \\012 ' '
polconversions=`cat $ers-jobs-map.txt | grep -v do.not | wc -l`
allifsplots=`ls -l $evs*polcon*/*TS/ALL*png | wc -l`
antabfiles=`ls -l $evs*polcon*/*ANTAB | wc -l`
[ $polconversions -eq $allifsplots ] || { echo -n '### missing plots ';
    echo $polconversions -ne $allifsplots '(polconversions ne allifplots)' ; }
[ $polconversions -eq $antabfiles ] || { echo -n '### missing antabfiles ';
    echo $polconversions -ne $antabfiles '(polconversions ne $antabfiles)' ; }
# generate a list of what is missing (2>&1 >/dev/null captures stderr only)
for j in `cat $ers-jobs-map.txt | grep -v do.not | awk '{print $1}'`
    do ls *$j*/*ANTAB *$j*/*TS/A*png 2>&1 >/dev/null | sed 's/^/### /' ; done
# paste ### lines here -- these are the jobs missing plots or antabs --
# ideally these should be investigated and resolved.

# now review PolConvert progress by job:
summarizePolconvertLogs.py -s -c -g 0.5 -b 0.8
# The -g and -b values set thresholds for
# the line between good/poor and poor/bad; the defaults (0.3 and 0.6)
# are really only likely to get you 'good' for a realy bright source
# (e.g. 3C279) as these are fringes of a single channel.
#
# Generate a list of some of the questionable cases to look at; paste it here:
summarizePolconvertLogs.py -s -g 0.5 -b 0.8 |\
grep -v good:[34] | awk '{print "### doyhhmm=" $3, "#",$2,$5,$4}'
# and examine some of the 4fit fringes on questionable cases
# (using doyhhmm=.. from above, or other cases as you like) with:
fplot $ers-*-4fit.$expn.save/$doyhhmm/A[^A].B.*
#   doyhhmm=doy-hhmm # replace job/qual/target with sort comments

# if comparisons are available (with previous releases or iterations)
compare-baselines-v6.pl -n 10000 -m 10 -f -x AL \
    -a ...4fit.$expn.save/*.alist -b ...4fit.$expn.save/*.alist

# verify that fits files are missing what is sensible
# (delete lines  that are only x because of missing stations)
cat *fits*/*pclist | egrep ' AA |x ' | sort | uniq |\
    grep -v '#' > $ers-fits-missing.txt
cat *fits*/*pclist | egrep ' AA |x |o ' | sort | uniq |\
    grep -v '#' > $ers-fits-allscans.txt
cp -p $ers-fits-missing.txt $release/logs
cat $ers-fits-missing.txt | sed 's/^/### /'
# and paste it here

# Final steps ======================
# generate some summary aedit pdfs and look at them
$ehtc/ehtc-aeditjob.sh all
[ `cat $ers-$expn.errors|wc -l` -ge "37" ] || echo check alist generation
cp -p $ers-$expn-*-time.pdf $release/logs
cp -p $ers-$expn.alist  $release/logs

# verify that the per-scan antabs are in agreement with the QA2 estimates:
# environment variables decimation, plotrange, dayoffset may be needed
for pc in $plst
do  $ehtc/ehtc-antab.sh $subv $pc $ers true
    cp -p $ers-$pc-antab.pdf $release/logs ; done
# if you need to diagnose bad behavior, do it with verbose commentary:
# verb=true $ehtc/ehtc-antab.sh ...

# generate a single antab for the track, and plot it as above.
antab=$ers-AA.antab
$ehtc/ehtc-track-antab.py -a $antab
for pc in $plst
do  $ehtc/ehtc-antab.sh $subv $pc $ers true $antab ; done
# review $antab*pdf and if acceptable, post it to $release/meta
# this file may be slightly shorter as it eliminates the no-data ANTABS.
[ -d $release/meta ] || mkdir $release/meta
cp -p $antab $antab*pdf $release/meta

# check on progress/missing scans (incrementally or when done):
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -c $exp.codes -K |\
  sort -k 1n > $ers-manifest.txt
cp -p $ers-manifest.txt $release/logs
awk '$4 == $6 {next;}{print;}' $ers-manifest.txt | sed 's/^/### /'

# when ready to release, check on space in Archive:
du -sBG tb-* | awk '{t+=$1}END{print t "G"}'
df -h $release
# if there is more than enough space: execute in parallel:
for r in tb-* ; do pushd $r ; nohup ./release.sh & disown ; popd ; done
# if you aren't sure, execute them serially
for r in tb-* ; do pushd $r ; nohup ./release.sh & wait   ; popd ; done &
# monitor (if you want) with
du -sBG tb-*

# and finally after everything is released count the products
$ehtc/ehtc-release-check.sh | sed 's/^/### /'

# after tarballs are delivered you can remove the polconvert swin dirs:
rm -rf $exp-$vers-${subv}_????.{save,difx}

# one last time
logfile=$exp-$subv-v${vers}${ctry}${stry}p${iter}r${relv}.logfile
comment=$exp-$subv-v${vers}${ctry}${stry}p${iter}r${relv}.comment
grep '^###' $logfile > $comment
cp -p $logfile $comment $release/logs
ls -l $release/logs

# Cleanup list ======================
# the product dirs should probably be saved until the archive is final...
ls -ld $exp-*-${subv}*.save
# ...but you can remove them too:
rm -rf $exp-*-${subv}*.save
}
# avoid worrisome error return values
true

#
# eof vim: nospell noai
#
