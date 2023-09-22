#!/bin/bash
#
# A script that can be used to switch an existing nightly build to one
# that uses e.g. branches/py3temp sources.  Useful to destroy a previous
# nightly build and save the time of a complete DIFX rebuild.
#
[ -n "$DIFXROOT" ] || {
    echo you need to source setup first
    exit 1
}
branch=branches/py3temp
svn=`dirname $DIFXROOT`/difx-svn
bld=${DIFXROOT/root/bld}
echo 
echo hacking up $DIFXROOT
echo using $svn/setup/install-difx
echo to reinstall from $bld using $branch
echo
cd $bld
pwd

echo
echo cleaning up ...
echo cd $bld/applications/polconvert/$branch \&\& make clean
( cd $bld/applications/polconvert/$branch ; make clean )
echo rm -rf $DIFXROOT/share/polconvert

echo build and install

$svn/setup/install-difx \
    --doonly polconvert --newver=polconvert:$branch --nodoc

echo installed share polcovert, these should all be new
ls -l $DIFXROOT/share/polconvert

echo installed share hops, these should not be new
ls -l $DIFXROOT/share/hops

echo
echo done
echo
