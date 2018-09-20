#
# Canned instructions for polconversion and packaging for ...
#
# This version is appropriate to Cycle4 beginning with Rev5
# The template for this file is in:     $ehtc/Readme-Cycle4.txt
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
#source /swc/difx/setup-DiFX-2.5.2.bash
#source /swc/difx/setup-DiFX-2.5.bash
source /swc/difx/setup-difx.bash
#source /swc/difx/difx-root-??mmm??/setup-difx.bash
#source /swc/hops/hops.bash
source $DIFXROOT/bin/hops.bash

# site vars: these point to the mirror or difx SVN tree
export hays=/data-sc24/EHT_ARCHIVE/Hays_Output2
export bonn=/data-sc24/EHT_ARCHIVE/Bonn_Output2
export dsvn=/swc/difx/difx-svn
# site vars: script area, correlator work dir and release directory
#export ehtc=$dsvn/sites/Haystack/ehtc
export ehtc=/swc/scripts/ehtc
export arch=$hays
export corr=/data-sc05/difxoper
export work=/data-sc15/difxoper

# principal vars for tracking all the revisions and forth
export exp=e17...
export vers=?       # major correlator version
export ctry=''      # minor correlator version
export subv=lo      # hi ( b1 b2 b3 b4 )
export iter=5       # numbered consecutively
export relv=5       # archive release name
export flab=''      # re-fourfitting version
export expn=3...    # HOPS experiment number (from Mike Titus)

# polconvert variables and other option variables
# $pdir depend on QA2 development
# $dpfu is estimated from QA2 products using this:
# casa --nologger --nologfile -c $DIFXROOT/share/polconvert/DPFU_scanner.py
# from which a single dpfu (degrees / flux unit) should be estimated for
# the track or campaign.
export pcal=TRACK_?
export pdir=$hays/$exp/$exp-$relv/qa2
export dpfu=0.0308574
export scmp='PV,AZ,SM,AP,LM,SR,SP'
export opts="-r -P15 -S $scmp -f 4 -A $dpfu -q v8"
export fitsname=false
export aeditjob=$ehtc/ehtc-aeditjob.sh

# derived vars
export release=$arch/$exp/$exp-$relv
export dout=$corr/$exp/v${vers}$ctry
export evs=$exp-$vers-$subv
export ers=$exp-$relv-$subv
export ptar=$pcal.APP_DELIVERABLES.tgz

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
cd $work/$exp/v${vers}${ctry}p${iter}

# ONE TIME SETUP =================
false && { # ONE TIME SETUP
[ -d $work/$exp/v${vers}${ctry}p${iter} ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}
[ -d $release ] || mkdir -p $release
cd $work/$exp/v${vers}${ctry}p${iter}

cp -p $ehtc/ehtc-cycle4.codes $exp.codes
cp -p $dout/*vex.obs .

# haxp is generated in $dout so preserve $expn if found:
[ -d $dout/$expn ] && mv $dout/$expn $dout/$expn.save

# ehtc-tarballs.sh haxp expects *.codes in $dout otherwise it fails silently
cp -p $ehtc/ehtc-cycle4.codes $dout/$exp.codes

# clean slate fourfit control file
cat > $ers.bare <<EOF
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
cp -p $ers.bare $ers.conf
# may need to edit the above or just use one of these
#ff_conf=`ls -t $corr/$exp/v${vers}*p*/$evs.conf | head -1`
ff_conf=`ls -t $work/$exp/v${vers}*p*/$evs.conf | head -1`
[ -f "$ff_conf" ] && cp -p $ff_conf $ers.conf && ls -l $ers.conf

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

# this needs to be coordinated with the other correlator
[ -d $pdir ] || { mkdir -p $pdir && echo need DELIVERABLES tarball in $pdir ; }
tar zxf $pdir/$ptar
ls -ld $pcal.*

# Review README.DRIVEPOLCONVERT: it may specify something other than 'v8'
# which is the value passed as the -q argument to drivepolconvert.py
# via the opts='...' assignment above.  Make sure you have this right.
echo $opts ; cat README.DRIVEPOLCONVERT

# Proceed to the EXECUTION NOTES section begining with the TODO list,
# placing all commands and commentary between the #---------... lines.
} # ONE TIME SETUP

# GENERAL PROCESSING TEMPLATE ======================
false && {
# This section provides the template of commands for EXECUTION when
# you need to proceed with collections of jobs, especially if you must
# generate a fourfit control file (and indeed, you just need a few jobs).
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
# and complete as instructed
# B: thereafter, where you just re-use it.
$ehtc/ehtc-postdrive.sh echo $jobs
$ehtc/ehtc-postdrive.sh eval $jobs
#--------------------------------------------------------------------------
# archive to output (after reviewing/checking some of the products)
ls -lh ./tarballs/*
[ -d $release/$proj-$class ] || mkdir $release/$proj-$class
mv -v ./tarballs/$ers-$proj-$targ*tar $release/$proj-$class
ls -lh $release/$proj-$class/$ers-$proj-$targ*tar
# send email to GBC if tarballs are to be released outside of Corr WG
} # GENERAL PROCESSING TEMPLATE

# GRINDING PROCESSING TEMPLATE ======================
# This section shows the commands needed for the bulk grinding script.
# A: until you have the fourfit control file built, two passes:
false && {
export proj=yyy targ=XXX class=cal|sci|eht  ; export label=$proj-$targ
nohup $ehtc/ehtc-jsgrind.sh false true  < /dev/null > $label.log 2>&1
} # wait for it to finish, generate the control file, and then:
false && {
nohup $ehtc/ehtc-jsgrind.sh false false < /dev/null > $label.log 2>&1
}
# B: you have the control file; multiple grinds can be included
false && {
export proj=yyy targ=XXX class=cal|sci|eht  ; export label=$proj-$targ
nohup $ehtc/ehtc-jsgrind.sh < /dev/null > $label.log 2>&1
} & disown
# disown ensures that the script will not die when your shell does.
# Reorder the TODO list generated by ehtc-joblist...-L as appropriate.
# GRINDING PROCESSING TEMPLATE

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
# While processing ======================
# save logfile incrementally or when done:
false && {
cp -p $exp-$subv-v${vers}${ctry}p${iter}r${relv}.logfile $release/logs
ls -l $release/logs

# review PolConvert progress incrementally or when done:
summarizePolconvertLogs.py -s -c # -g 0.5 -b 0.8
# The -g and -b values set thresholds for
# the line between good/poor and poor/bad; the defaults (0.3 and 0.6)
# are really only likely to get you 'good' for a realy bright source
# (e.g. 3C279) as these are fringes of a single channel.  Also:
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -T | tee $ers-performance.txt

# Examine some of the 4fit fringes on questionable cases with
fplot $ers-$label-4fit.$expn.save/doy-hhmm/A[^A].B.*

# if comparisons are available (with previous releases)
compare-baselines-v6.pl -n 10000 -f -x BL \
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

# Cleanup list ======================
# after tarballs are delivered and if you want to recover disk space
rm -rf $exp-$vers-${subv}_*.save
rm -rf $exp-$relv-${subv}_*.save
}

#
# eof
#
