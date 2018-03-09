#
# Canned instructions for polconversion and packaging for ...
#
# This version is appropriate to Cycle4 through Rev3
#
# Make a copy for each band/track and customize the environment
# variables at the top of the ENVIRONMENT section.
#

# ENVIRONMENT =====================
# go to working host and setup tools -- different per site
to eht
casadev   # alias that adds CASA 4.7.2 bin to PATH
#source /swc/difx/setup-DiFX-2.5.bash
#source /swc/difx/setup-difx.bash
source /swc/difx/difx-root-18Jan09/setup-difx.bash
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
export exp=e17d05
export vers=3
export ctry=''
export subv=lo
export iter=2
export relv=3
export flab=''
export expn=3597

# the polconvert variables
export pcal=TRACK_D
export pdir=$hays/$exp/$exp-$vers/qa2
export dpfu=0.0308574
export scmp='PV,AZ,SM,AP,LM,SR,SP'
export opts="-r -S $scmp -f 4 -A $dpfu"

# derived vars
export release=$arch/$exp/$exp-$relv
export dout=$corr/$exp/v${vers}$ctry
export evs=$exp-$vers-$subv
export ptar=$pcal.APP_DELIVERABLES.tgz

# check:
[ `( 
echo =============================================================== && \
echo $exp $vers .$ctry. $subv $iter $relv .$flab. $expn && \
echo $evs $opts && \
echo $dout && \
echo $ptar $pcal && \
echo $pdir && \
echo $release && \
echo =============================================================== && \
type casa && \ 
type mpifxcorr && \
type fourfit && \
echo =============================================================== 
) | wc -w` -eq 33 ] && echo variables are ok || echo issue with variables

# Nth TIME SETUP =================
cd $work/$exp/v${vers}${ctry}p${iter}

# ONE TIME SETUP =================
[ -d $work/$exp/v${vers}${ctry}p${iter} ] ||
    mkdir $work/$exp/v${vers}${ctry}p${iter}
[ -d $work/$exp/v${vers}tb ] ||
    mkdir $work/$exp/v${vers}tb
[ -d $release ] || mkdir $release
cd $work/$exp/v${vers}${ctry}p${iter}
tar zxf $pdir/$ptar

cp -p $ehtc/ehtc-template.codes $exp.codes
cp -p $dout/*vex.obs .
# cp -p $dout/*joblist .

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
ff_conf=`ls -tr $corr/$exp/v${vers}*p*/$evs.conf | head -1`
ff_conf=`ls -tr $work/$exp/v${vers}*p*/$evs.conf | head -1`
[ -f "$ff_conf" ] && cp -p $ff_conf . && ls -l $evs.conf

# if haxp is to be generated
cp -p $exp.codes $dout

# move this aside if it exists:
ls -ld $dout/$expn
mv $dout/$expn $dout/$expn.save

# generate TODO list with:
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -G

# maintain notes while you execute and post this file periodically to
[ -d $release/logs ] || mkdir $release/logs

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
ls -lh ../*/tarballs/*
[ -d $release/$proj-$class ] || mkdir $release/$proj-$class
mv ../*/tarballs/* $release/$proj-$class
ls -lh $release/$proj-$class/$evs-$proj-$targ*tar
# send email to GBC if tarballs are to be released outside of Corr WG

# EXECUTION NOTES ======================

# paste all commands as at the shell prompt
# pause to observe correct progress and make comments:
#   # ... details of interest to correlator folks
#   ###  important messages that can be grepped out to make a summary

#--------------------------------------------------------------------------
# done.
# save logfile:
# cp -p $exp-$subv-v${vers}${ctry}p${iter}.logfile $release/logs
#
#--------------------------------------------------------------------------
# TODO list ======================
# $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -G

# Cleanup list ======================
rm -rf $exp-$vers-${subv}_*.save

#
# eof
#
