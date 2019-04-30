#
# Instructions for polconversion/packaging for <Observation><Band>
#
# This version is appropriate to Cycle5 (b1..b4)
# The template for this file is in:     $ehtc/Readme-Cycle5.txt
# rename it to:      $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile
#
# Make a copy for each band/track and customize the environment
# variables at the top of the ENVIRONMENT section.
#  v${vers}${ctry}  refers to the DiFX correlation output used
#  p${iter}         refers to the polconvert iteration
#  r${relv}         refers to the release name
#
# Cut and paste from this file (which is necessary to get started).
# Once the fourfit control file is in hand, you can execute parts of
# the file using true && { ... } or false && { ... } for partial runs.
#
# Post copies of this file and any grinding logs to $release/logs.
#

# ENVIRONMENT =====================
# go to working host and cut & paste or pipe it:
# cat this.logfile | ssh -2x host & disown
# ---
# setup versioned tools -- different per site
# script that adds CASA 4.7.2 bin to PATH
source ~/lib/casa.setup
source /swc/difx/setup-DiFX-2.5.3.bash
#source /swc/difx/setup-DiFX-2.5.2.bash
#source /swc/difx/setup-DiFX-2.5.bash
#source /swc/difx/setup-difx.bash
#source /swc/difx/difx-root-YYmonDD/setup-difx.bash
#source /swc/hops/hops.bash
# Only if you had somehow previously set it up:
# export HOPS_SETUP=false
source $DIFXROOT/bin/hops.bash

# site vars: these point to the mirror or difx SVN tree
export hays=/data-sc24/EHT_ARCHIVE/Hays_Output2
export bonn=/data-sc24/EHT_ARCHIVE/Bonn_Output2
export dsvn=/swc/difx/difx-svn
export dsvn=/swc/difx/difx-svn/master_tags/DiFX-2.5.3/
# site vars: script area, correlator work dir and release directory
#export ehtc=/sites/Haystack/ehtc
export ehtc=$dsvn/sites/Haystack/ehtc
#export ehtc=/swc/scripts/ehtc
export arch=$hays
export corr=/data-sc15/difxoper
# run polconvert on the same machine with the files
export work=/data-sc15/difxoper

# If $work contains multiple jubs or unprocessed jobs, set this to true,
# which implicitly sets the -u flag on any use of $ehtc/ehtc-joblist.py.
# If $work is more messed up than that, you're on your own.
export uniq=true    # or export uniq=false

# principal vars for tracking all the revisions and forth
export exp=e18...
export vers=?       # major correlator version
export ctry=''      # minor correlator version
export subv=b?      # b1 b2 b3 b4
export iter=?       # numbered consecutively
export relv=?       # archive release name number
export flab=''      # re-fourfitting version (if needed)
export expn=3...    # HOPS exp # (from Mike Titus)

# polconvert variables and other option variables
# $pdir depend on QA2 development
# $dpfu is estimated from QA2 products using this:
# casa --nologger --nologfile -c $DIFXROOT/share/polconvert/DPFU_scanner.py
# from which a single dpfu (degrees / flux unit) should be estimated for
# the track or campaign.
export pcal=TRACK_?                            # per QA2 delivery
export pdir=$hays/$exp/$exp-$vers/qa2
export dpfu=0.0308574                          # ave over session, val TBD
export scmp='PV,MG,SW,AX,LM,SZ'                # cycle5
export opts="-r -P15 -S $scmp -f 4 -A $dpfu -q v8"
export fitsname=false
export aeditjob=$ehtc/ehtc-aeditjob.sh

# derived vars
export release=$arch/$exp/$exp-$relv
export dout=$corr/$exp/v${vers}$ctry/$subv
export evs=$exp-$vers-$subv
export ers=$exp-$relv-$subv
export ptar=$pcal???label???.APP_DELIVERABLES.tgz

# check:
wordcount=`(
echo =============================================================== && \
echo $exp $vers .$ctry. $subv $iter $relv .$flab. $expn && \
echo $evs $ers $opts $fitsname $aeditjob && \
echo $dout && \
echo $ptar $pcal && \
echo $pdir && \
echo $release && \
echo =============================================================== && \
type casa && \
type mpifxcorr && \
type fourfit && \
echo =============================================================== 
) | wc -w`
[ "$wordcount" -eq 39 ] && echo variables are ok || echo issue with variables

# Nth TIME SETUP =================
cd $work/$exp/v${vers}${ctry}p${iter}/$subv

# ONE TIME SETUP =================
false && { # ONE TIME SETUP
[ -d $work/$exp/v${vers}${ctry}p${iter} ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}
[ -d $work/$exp/v${vers}${ctry}p${iter}/$subv ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}/$subv
[ -d $release ] || mkdir -p $release
cd $work/$exp/v${vers}${ctry}p${iter}/$subv

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
[ -d ../qa2 ] || { mkdir ../qa2 ; pushd ../qa2 ; tar zxf $pdir/$ptar ; popd ; }

# link in the QA2 package tables for this band
for d in ../qa2/$pcal.* ; do ln -s $d . ; done
for f in ../qa2/README.* ; do ln -s $f . ; done
ls -ld $pcal.*

# Review README.DRIVEPOLCONVERT: it may specify something other than 'v8'
# which is the value passed as the -q argument to drivepolconvert.py
# via the opts='...' assignment above.  Make sure you have this right.
echo $opts ; cat README.DRIVEPOLCONVERT

# pull in the experiment codes
cp -p $ehtc/ehtc-template.codes $exp.codes
cp -p $dout/*vex.obs .
[ `ls -l *vex.obs | wc -l` -eq 1 ] || echo Too many/too few vex.obs files

# haxp is generated in $dout so preserve $expn if found:
[ -d $dout/$expn ] && mv $dout/$expn $dout/$expn.save

# ehtc-tarballs.sh haxp expects *.codes in $dout otherwise it fails silently
cp -p $ehtc/ehtc-template.codes $dout/$exp.codes

# clean slate fourfit control file
cat > $ers.bare <<EOF
* bare fourfit config file for ALMA 1mm session April 2018 $ers
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
# $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile

# provide a number of summary reports prior to any grinding
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -B > $ers-bl-pol-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -D > $ers-ant-ch-bl-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -F > $ers-ant-ch-map.txt
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -R > $ers-jobs-map.txt
( cd $dout ; summarizeDifxlogs.py    ) > $ers-difxlog-sum.txt
( cd $dout ; summarizeDifxlogs.py -c ) > $ers-difxlog-clr.txt
cp -p $ers*.txt $release/logs
cp -p $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile $release/logs

#
# run the GENERAL PROCESSING commands on one or three jobs to make
# suitable data for generating "good enough" manual phase cals
#
### Notes on building $ers.conf
# pick scans to use grep from *jobs*
# stations: ...
awk '{print $5}' $ers-jobs-map.txt | tr '-' \\012 | sort | uniq -c
# types of baselines: ...
awk '{print $5}' $ers-jobs-map.txt | sort | uniq -c

jobs=`echo $exp-$vers-${subv}_{,}.input` ; echo $jobs
prepolconvert.py -v -k -s $dout $jobs
drivepolconvert.py -v $opts -l $pcal $jobs
for j in $jobs ;\
do difx2mark4 -e $expn -s $exp.codes --override-version ${j/input/difx} ; done


# identify roots:
roots=`cd $expn ; ls */target*` ; echo $roots
cd $expn ; cp ../$ers.conf .
# For each root run this command, but set -s argument
# with a comma-sep list of single letter station codes
# that should be fit (relative to A as first station).
# Refer to the station codes file for the 2-letter to 1-letter codes.
$ehtc/est_manual_phases.py -c $ers.conf \
    -r first-root -s A,...
grep ^if.station $ers.conf | sort | uniq -c
$ehtc/est_manual_phases.py -c $ers.conf \
    -r second-root -s A,...
grep ^if.station $ers.conf | sort | uniq -c
#...

# be sure to clean up afterwards, especially to move $expn aside
cd ..
cp -p $expn/$ers.conf .
cp -p $expn/$ers.conf $release/logs
mv $expn ff-conf-$expn
rm -rf ${jobs//input/*}
### now have $ers.conf
} # ONE TIME SETUP

# GENERAL PROCESSING TEMPLATE ======================
# (deleted; see Readme-Cycle4.txt)

# GRINDING PROCESSING TEMPLATE ======================
# (deleted; see Readme-Cycle4.txt)

# EXECUTION NOTES ======================
# Capture all commands in this file.
# Use true && { ... } for blocks of commands to be executed as a group.
# then change true to false to prevent re-execution on the next group.
# As needed insert comments (for grepping later):
#   # ... details of interest to correlator folks
#   ###  important messages that can be grepped out to make a summary
#--------------------------------------------------------------------------
# TODO list ======================
# $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -L

###
### log of $ers commands goes here
###


# you can launch as a remote job with:
# cat this.logfile | ssh -2x host & disown
# or on the processing host with:
# sh this.logfile & disown
#--------------------------------------------------------------------------
#
# Final Steps ======================
# This section is always MANUL.
#
false && {
# save logfile incrementally or when done:
cp -p $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile $release/logs
ls -l $release/logs

# capture the performance on polconversion for future reference
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -T | tee $ers-performance.txt
cp -p $ers-performance.txt $release/logs

# review PolConvert progress incrementally or when done:
summarizePolconvertLogs.py -s -c -g 0.5 -b 0.8
# The -g and -b values set thresholds for
# the line between good/poor and poor/bad; the defaults (0.3 and 0.6)
# are really only likely to get you 'good' for a realy bright source
# (e.g. 3C279) as these are fringes of a single channel.  Also:

# additional checks for polconvert issues
grep -l SEVERE *pol*/casa*/*output |\
    cut -d_ -f2 | cut -d. -f1 | tr \\012 ' '
grep -l 'was NOT polconverted properly' *polcon*/casa-logs/*output |\
    cut -d_ -f2 | cut -d. -f1 | tr \\012 ' '
polconversions=`cat $ers-jobs-map.txt | grep -v do.not | wc -l`
allifsplots=`ls -l $ers*polcon*/*TS/ALL*png | wc -l`
antabfiles=`ls -l $ers*polcon*/*ANTAB | wc -l`
[ $polconversions -eq $allifsplots ] || { echo -n '###' missing plots;
    echo $polconversions -ne $allifsplots '(polconversions ne allifplots)' ; }
[ $polconversions -eq $antabfiles ] || { echo -n '###' missing antabfiles;
    echo $polconversions -ne $antabfiles '(polconversions ne $antabfiles)' ; }

# Examine some of the 4fit fringes on questionable cases with
fplot $ers-*-4fit.$expn.save/$doyhhmm/A[^A].B.*

# if comparisons are available (with previous releases)
compare-baselines-v6.pl -n 10000 -f -x AL \
    -a ...4fit.$expn.save/*.alist -b ...4fit.$expn.save/*.alist

# verify that fits files are not missing data or report on it:
cat *fits*/*pclist
cat *fits*/*pclist | egrep '   AA|x ' | uniq | sed 's/^/### /'

# Final steps ======================
# generate some summary aedit pdfs
$ehtc/ehtc-aeditjob.sh all
cp -p $ers-$expn-*-time.pdf $release/logs

# verify that the per-scan antabs are in agreement with the QA2 estimates:
$ehtc/ehtc-antab.sh $subv $pcal $ers true
cp -p $ers-$pcal-antab.pdf $release/logs

# check on progress/missing scans (incrementally or when done):
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -c $exp.codes -K |\
  sort -k 1n > $release/logs/$ers-manifest.txt

# when ready to release, execute these shells
ls tb-*/release.sh
for r in tb-* ; do pushd $r ; nohup ./release.sh & popd ; done

# and finally after everything is released count the products
$ehtc/ehtc-release-check.sh

# one last time
cp -p $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile $release/logs
ls -l $release/logs

# Cleanup list ======================
# after tarballs are delivered and if you want to recover disk space
rm -rf $exp-$vers-${subv}_*.save
rm -rf $exp-$relv-${subv}_*.save
}
# avoid worrisome error return values
true

#
# eof
#
