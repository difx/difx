#!/bin/sh
#
# Script to unpack HOPS3 data tarballs from MHO_REGRESSION_DATA.
#
# If MHO_REGRESSION_DATA and an approprate tarball is present
# then the tarball will be unpacked to a specified location
# ($nukable below, to emphasize that this can be regenerated).
#
# If MHO_REGRESION_DEST is set, then the unpack will put data in the
# in the MHO_REGRESION_DEST directory rather than MHO_REGRESSION_DATA
# and $nukable will refer to this destination.
#
# If MHO_REGRESSION_EXTRACT is set to 'false' then the name of the
# tarball is returned as 'mhoregtgz=...'.  This prevents accidentally
# nuking the tarball, and the tarball name is available with an eval.
#
# The companion script to create the tarballs is part of the HOPS4
# GIT repo as part of the migration software suite.
#
# This companion script to unpacks according to its configuration
# (which must be synchronized with the tarball creator, legacy_tar.sh).
# That is, it does a lookup on any requested tarball and verifies that
# it either exists (i.e. previously unpacked) or needs to be unpacked.
# At the moment, unpacking is all or nothing, and that may suffice.
#
# If successfully executed, the $nukable directory is echoed, and
# an exit status of 0 is set.  This allows the caller to remove the
# test data if that is desirable.
#
# This script ends by echoing the name of the directory created
# (and thus something that may be removed) and sets exit status 0.
#  exit 0 if success (unpacked)
#  or 1 for failure
#  or 77 for no data to unpack (SKIP)
#  or 99 for some configuration ERROR
name=${1-'help'}
[ "$name" = 'help' -o "$name" = '--help' ] &&
    { echo Usage: $0 name -- read script ; exit 1 ; }

# some abbreviations to make the case statement below more compact
# these are unpack directories
leg=$MHO_REGRESSION_DATA/tarballs/legacy
mhx=$MHO_REGRESSION_DATA/historical
mhs=$MHO_REGRESSION_DATA/released
mhr=$MHO_REGRESSION_DATA/reference

mff=$MHO_REGRESSION_DATA/ff_testdata
mae=$MHO_REGRESSION_DATA/ae_testdata

mhv=$MHO_REGRESSION_DATA/vgosdata

src=$name

### and need to finish import of sources

# should be the same list as in bootstrap/legacy_tar.sh
case $name in

    # various sources not likely to be duplicated in HOPS4
    corr)       parent=$mhx     ; odr=$src          ;;
    mk4m)       parent=$mhx     ; odr=$src          ;;
    ompi)       parent=$mhx     ; odr=$src          ;;
    difx)       parent=$mhx     ; odr=$src          ;;
    mark5)      parent=$mhx     ; odr=$src          ;;
    h3msrc)     parent=$mhx     ; odr=$src          ;;
    h3mtst)     parent=$mhx     ; odr=$src          ;;
    readme)     parent=$mhx     ; odr=$src          ;;
    swcmsc)     parent=$mhx     ; odr=$src          ;;
    swcmsc)     parent=$mhx     ; odr=$src          ;;
    tarpub)     parent=$mhs     ; odr=$src          ;;
    tarprv)     parent=$mhs     ; odr=$src          ;;

    # sources that are re-worked in HOPS4
    doc)        parent=$mhr     ; odr=$src          ;;
    h3help)     parent=$mhr     ; odr=$src          ;;
    include)    parent=$mhr     ; odr=$src          ;;
    pphops3)    parent=$mhr     ; odr=$src          ;;
    pphops4)    parent=$mhr     ; odr=$src          ;;
    scripts)    parent=$mhr     ; odr=$src          ;;
    sublib)     parent=$mhr     ; odr=$src          ;;

    # ff_testdata subdirs
    misc)       parent=$mff     ; odr=$src          ;;
    2491)       parent=$mff     ; odr=$src          ;;
    2611)       parent=$mff     ; odr=$src          ;;
    2836)       parent=$mff     ; odr=$src/scan001  ;;
    2843)       parent=$mff     ; odr=$src/321-1701_0552+398  ;;
    2849)       parent=$mff     ; odr=$src          ;;
    2912)       parent=$mff     ; odr=$src          ;;
    3064)       parent=$mff     ; odr=$src          ;;
    3262)       parent=$mff     ; odr=$src          ;;
    3365)       parent=$mff     ; odr=$src          ;;
    3372)       parent=$mff     ; odr=$src          ;;
    3413)       parent=$mff     ; odr=$src          ;;
    3562)       parent=$mff     ; odr=$src          ;;
    3571)       parent=$mff     ; odr=$src          ;;
    3727)       parent=$mff     ; odr=$src          ;;
    3756)       parent=$mff     ; odr=$src          ;;
    3772)       parent=$mff     ; odr=$src          ;;
    average)    parent=$mff     ; odr=$src          ;;
    # ae_testdata subdirs
    aetest)     parent=$mae     ; odr=$src          ;;
    3593)       parent=$mhv     ; odr=$src          ;;

    # vex2xml
    v2xsrc)     parent=$mhr     ; odr=$src          ;;
    v2xtst)     parent=$mhr     ; odr=$src          ;;
    *)          echo unconfigured $name ; exit 2    ;;
esac

# For the purposes of testing in the bootstrap/Makefile, we want to provide
# an ability to replace MHO_REGRESSION_DATA with a new (FAKE) location.
[ -n "$MHO_REGRESSION_DEST" ] &&
    parent=`echo $parent | sed "s,$MHO_REGRESSION_DATA,$MHO_REGRESSION_DEST,"`

nukable=$parent/$odr

# successfully found -- we are done
[ -d "$nukable" ] && echo "$nukable" && exit 0

# this is where the tarballs are stored.
tgz=$leg/$src.tar.gz

# it is a SKIP if the required tarball is missing
[ -f "$tgz" ] || { echo source $tgz is missing; exit 77; }
[ "x$MHO_REGRESSION_EXTRACT" = xfalse ] && { echo mhoregtgz="$tgz" ; exit 0 ; }

# it might not yet exist
[ -d "$parent" ] || mkdir -p $parent

# ok, actually extract it
cwd=`pwd`
cd $parent && tar zxf $tgz
cd $cwd

# successfully unpacked -- we are done
[ -d "$nukable" ] && echo "$nukable" && exit 0

exit 99
#
# eof
#
