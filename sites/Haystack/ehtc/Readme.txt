These scripts and artifacts are used to package the EHTC data.

Assuming the data is organized by project, target and class,
the sequence of commands below will process one proj/targ/class.
The list of such may be generated with

  $ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs -G

I.e. each line of output specifies one proj/targ/class assignment.
Then for each such assignment, 

export proj=yyy targ=XXX class=cal|sci|eht
export jselect="-p $proj -s $targ"
export label=$proj-$targ
eval `$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -J`
# sets jobs=''
$ehtc/ehtc-joblist.py -i $dout/$evs -o *.obs $jselect -R
# lists the jobs
export scmp='PV,AZ,SM,AP,LM,SR,SP'
export opts="-r -S $scmp -f 32"
prepolconvert.py -v -k -s $dout $jobs
# evaluate partitioning of job list (use -v if error)
$ehtc/ehtc-zoomchk.py $jobs
# subdivide $jobs as necessary, do the polconvert on each set of $jobs
drivepolconvert.py -v $opts -l $pcal $jobs
# evaluate results on full set of $jobs--look at ALL_IFs plots; then
#
# A: you already have the fourfit conf file built, you can 'eval'
$ehtc/ehtc-postdrive.sh echo false $jobs
$ehtc/ehtc-postdrive.sh eval false $jobs
# B: otherwise, run through 4fit, work the pc phases, then run 4fit again.
# cd $expn ; $ehtc/est_manual_phases.py -c $evs.conf <root> ; cd ..
$ehtc/ehtc-postdrive.sh echo $jobs
$ehtc/ehtc-postdrive.sh eval $jobs
#
# archive to output
[ -d $archive/$proj-$class ] || mkdir $archive/$proj-$class
mv ../*/tarballs/* $archive/$proj-$class

eof
