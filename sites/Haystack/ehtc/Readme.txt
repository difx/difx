These scripts and artifacts are used to package the EHTC data.

Assuming the data is to be organized by project, target and class,
the sequence of commands below will process one proj/targ/class.
The list of such may be generated with

  $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -G

I.e. each line of output specifies one proj/targ/class assignment.
Then for each such assignment, 

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
# A: you already have the fourfit conf file built, you can 'eval'
$ehtc/ehtc-postdrive.sh echo false $jobs
$ehtc/ehtc-postdrive.sh eval false $jobs
# B: otherwise, run through 4fit, work the pc phases, then run 4fit again.
# cd $expn ; $ehtc/est_manual_phases.py -c $evs.conf <root> ; cd ..
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

eof
