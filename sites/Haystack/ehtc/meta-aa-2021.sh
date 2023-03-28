#!/bin/bash
#
# Simple script to assemble antabs into a single tarball;
# all files referenced are linked into a working directory
# whose contents are tarred up (with -h for dereference).
#
# variables defining content, absolute paths
#
# for 2022, 2023, ... make a copy and edit appropriately.
#
antabs="
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21a14/e21a14-1/meta/e21a14-1-b1-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21a14/e21a14-1/meta/e21a14-1-b4-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21a17/e21a17-1/meta/e21a17-1-b4-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21d15/e21d15-1/meta/e21d15-1-b1-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21d15/e21d15-1/meta/e21d15-1-b4-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21e13/e21e13-1/meta/e21e13-1-b1-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21e13/e21e13-1/meta/e21e13-1-b4-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21e18/e21e18-1/meta/e21e18-1-b4-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21f19/e21f19-1/meta/e21f19-1-b1-Aa-v1b.antab
/data-sc25/EHT_ARCHIVE/Bonn_Output3/e21f19/e21f19-1/meta/e21f19-1-b4-Aa-v1b.antab

/data-sc04/EHT_ARCHIVE/Hays_Output6/e21a14/e21a14-1/meta/e21a14-1-b3-AA.antab
/data-sc04/EHT_ARCHIVE/Hays_Output6/e21a17/e21a17-1/meta/e21a17-1-b3-AA.antab
/data-sc04/EHT_ARCHIVE/Hays_Output6/e21d15/e21d15-1/meta/e21d15-1-b3-AA.antab
/data-sc04/EHT_ARCHIVE/Hays_Output6/e21e13/e21e13-1/meta/e21e13-1-b3-AA.antab
/data-sc04/EHT_ARCHIVE/Hays_Output6/e21e18/e21e18-1/meta/e21e18-1-b3-AA.antab
/data-sc04/EHT_ARCHIVE/Hays_Output6/e21f19/e21f19-1/meta/e21f19-1-b3-AA.antab
/data-sc25/EHT_ARCHIVE/Hays_Output3/e21a14/e21a14-1/meta/e21a14-1-b2-AA.antab
/data-sc25/EHT_ARCHIVE/Hays_Output3/e21a17/e21a17-1/meta/e21a17-1-b2-AA.antab
/data-sc25/EHT_ARCHIVE/Hays_Output3/e21d15/e21d15-1/meta/e21d15-1-b2-AA.antab
/data-sc25/EHT_ARCHIVE/Hays_Output3/e21e13/e21e13-1/meta/e21e13-1-b2-AA.antab
/data-sc25/EHT_ARCHIVE/Hays_Output3/e21e18/e21e18-1/meta/e21e18-1-b2-AA.antab
/data-sc25/EHT_ARCHIVE/Hays_Output3/e21f19/e21f19-1/meta/e21f19-1-b2-AA.antab
"

# the file of ALMA metadata produced from logs & QA0+ products
aameta='/data-sc25/EHT_ARCHIVE/Hays_Output3/e21meta/EHT2021_metadata_AA.tar.gz'

# variables govering what is created
version=1.0
tarball=EHT2021_metadata_AA_$version
#destiny='/data-sc25/EHT_ARCHIVE/Hays_Output3/e21meta'
destiny='/data-sc04/EHT_ARCHIVE/Hays_Output6/e21meta'

# where we are working
workdir='wd'

# checking
[ -z "$install" ] && install=false
[ "$install" = 'true' -o "$install" = 'false' ] || {
    echo install must be true or false; exit 1 ; }

$install && echo finished tarball will be installed in $destiny as $tarball
$install || echo finished tarball will NOT be installed in $destiny as $tarball

$install && [ -f $destiny/$tarball ] && {
    echo $destiny/$tarball exists.
    echo you need to assign a new version
    exit 1
}

# sine qua non
[ -f $aameta ] || {
    echo ALMA metadata file $aameta does not exist
    echo cannot proceed
    exit 2
}

# might as well create the archive directory
[ -d $destiny ] || mkdir $destiny
[ -d $destiny ] || {
    echo $destiny does not exist ; exit 3; }

# do the work
[ -d $workdir ] && {
    echo removing previous $workdir && rm -rf $workdir || {
        echo unable to rm previous workdir ; exit 4; } ; }
[ -d $workdir ] || mkdir $workdir 
[ -d $workdir ] && chgrp difx $workdir && chmod 2775 $workdir || {
    echo permission issue with $workdir ;  exit 4 ; }
cd $workdir || {
    echo unable to cd to $workdir ; exit 4; }

for antab in $antabs $aameta
do
    # migrate files to uniform names for benefit of L1
    case $antab in
    *b1-Aa-v1b*) pattern=b1-Aa-v1b ; remap=b1-AA ;;
    *b4-Aa-v1b*) pattern=b4-Aa-v1b ; remap=b4-AA ;;
    *) pattern='' ; remap='' ;
    esac
    name=`basename $antab`
    [ -z "$pattern" ] && link=$name || link=${name/$pattern/$remap}
    [ -f $link ] && {
        echo finding a file in $workdir that should not exist ; exit 5 ; }
    [ -h $link ] && rm -f $link
    echo \
    ln -s $antab $link
    ln -s $antab $link
done

echo ; echo links made in $workdir ; echo

echo building tarball...
echo \
tar zcfh $tarball.tgz \*.antab \*.tar.gz
tar zcfh $tarball.tgz  *.antab  *.tar.gz
echo ... done
ls -l $tarball.tgz
# optional install
$install &&
    cp -p $workdir/$tarball.tgz $destiny && echo installed || {
        echo you may install $tarball.tgz with &&
        echo cp -p $workdir/$tarball.tgz $destiny
    }

echo ; echo once installed in $destiny you may remove $workdir ; echo

#
# eof vim: nospell
#
