#!/bin/bash
#
# $Id: chk_frqs.sh 4237 2024-03-22 14:17:09Z gbc $
#
# local test of frqs based on chk_ff_dfplot.sh
#
exp1=2843
exp2=3372

verb=false
[ -n "$testverb" ] && verb=true

# fourfit has many library dependencies that need to be found.
[ -d "$abs_top_builddir" ] ||
    { echo abs_top_builddir not set; exit 1; } 
ldlibpath=`export HOPS_QUIET=x ; . ../../hops.bash ; printenv LD_LIBRARY_PATH`
[ -n "$ldlibpath" ] && export LD_LIBRARY_PATH=$ldlibpath &&
    { echo setting LD_LIBRARY_PATH for fourfit ; echo $LD_LIBRARY_PATH ; }

[ -d "$abs_top_srcdir" ] ||
    { echo abs_top_srcdir not set; exit 1; }
$verb && echo \
export DATADIR=$abs_top_srcdir/data/ff_testdata/testdata
export DATADIR=$abs_top_srcdir/data/ff_testdata/testdata

[ -d $DATADIR/$exp1 ] ||
    { echo skipping since we have no data ; exit 77; }
[ -d $DATADIR/$exp2 ] ||
    { echo skipping since we have no data ; exit 77; }
$verb && echo have data in $DATADIR/$exp1
$verb && echo have data in $DATADIR/$exp2

[ -x ./fourfit ] ||
    { echo have no executable fourfit here ; exit 1; }
$verb && echo have an executable fourfit here

# for testing
[ -h $exp1 ] || { echo \
    ln -s $abs_top_srcdir/data/ff_testdata/testdata/$exp1 . && 
    ln -s $abs_top_srcdir/data/ff_testdata/testdata/$exp1 . ; }
[ -h $exp2 ] || { echo \
    ln -s $abs_top_srcdir/data/ff_testdata/testdata/$exp2 . && 
    ln -s $abs_top_srcdir/data/ff_testdata/testdata/$exp2 . ; }
export DEF_CONTROL=/dev/null
cat > frq1.cf <<EOF
* normal SX
* S: freqs a b c d e f
* X: freqs g h i j k l m n
frqs gh~n
pc_mode manual
est_pc_manual 1
fringeout_dir `pwd`/xxxx
EOF
rot2="$exp2/193-1757/0529+483.vtqbsq"
cat > frq2.cf <<EOF
* normal SX, but g and n are DSB
* S: freqs a b c d e f
* X: freqs g h i j k l m n
frqs g-~n+
pc_mode manual
est_pc_manual 1
fringeout_dir `pwd`/xxxx
dc_block true
if station T
 lsb_offset 305.0
 pc_phases_r g~n
*est_pc_manual doesn't know about sideband estimation....
*pc_phases_r ghijklmn
  +51.978 +179.505 +157.229 +112.320 -135.190  -33.459 +135.340 +131.803
EOF
rot3="$exp2/193-1757/0529+483.vtqbsq"
cat > frq3.cf << EOF
chan_ids ABCDEFGH
    8212.99 8252.99 8352.99 8512.99
    8732.99 8852.99 8912.99 8932.99
frqs B~G
pc_mode manual
est_pc_manual 1
fringeout_dir `pwd`/xxxx
if station T
 lsb_offset 305.0
* pc_phases_r A~H
* +51.978 +179.505 +157.229 +112.320 -135.190  -33.459 +135.340 +131.803
 pc_phases_r B~G
          +179.505 +157.229 +112.320 -135.190  -33.459 +135.340
EOF

export PARSEDEBUG=1
$verb && echo \
./fourfit -a -b AI:X -c frq1.cf $exp1
./fourfit -a -b AI:X -c frq1.cf $exp1 ||
    { echo unable to fourfit $exp1 ; exit 2 ; }
$verb && echo \
./fourfit -a -b TV:X -c frq2.cf $rot2
./fourfit -a -b TV:X -c frq2.cf $rot2 ||
    { echo unable to fourfit $exp2 ; exit 3 ; }
$verb && echo \
./fourfit    -b TV:X -c frq3.cf $rot3
./fourfit    -b TV:X -c frq3.cf $rot3 ||
    { echo unable to fourfit $rot3 ; exit 4 ; }

exit 0
#
# eof
#
