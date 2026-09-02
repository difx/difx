#!/bin/bash
#
# $Id: chk_clones.sh 4251 2024-03-25 15:07:08Z gbc $
#
# local test of cloning based on chk_frqs.sh
# Here we merely repeat the tests, but now with
# clone channels activated and using clone_snr_chk
# and display_chans as well.
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

export HOPS_PLOT_DATA_MASK=PDD_ALL

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
cat > cfrq1.cf <<EOF
* normal SX
* S: freqs a b c d e f
* X: freqs g h i j k l m n
frqs gh~n
* add a channel X as a clone of h
clone_ids kX
clone_snr_chk true
display_chans i~X
pc_mode manual
est_pc_manual 1
* note new channel below
if station A
 pc_phases_r ghijklmnX
 +137.080 +120.245 -103.609  +89.343  +32.263 +101.693 +148.370 -179.360 +32.263
fringeout_dir `pwd`/xxxx
plot_data_dir xxxx/cfrq1
EOF
rot2="$exp2/193-1757/0529+483.vtqbsq"
cat > cfrq2.cf <<EOF
* normal SX, but g and n are DSB
* S: freqs a b c d e f
* X: freqs g h i j k l m n
frqs g-~n+
* add a channels YZ as clones of mn
clone_ids mnYZ
clone_snr_chk true
display_chans g~mY@1
pc_mode manual
est_pc_manual 1
fringeout_dir `pwd`/xxxx
plot_data_dir xxxx/cfrq2
dc_block true
if station T
 lsb_offset 305.0
*pc_phases_r g~n
*est_pc_manual doesn't know about sideband estimation....
*pc_phases_r ghijklmn
*  +51.978 +179.505 +157.229 +112.320 -135.190  -33.459 +135.340 +131.803
if station T
 pc_phases_r ghijklmnYZ
  +83.975 +147.255 +125.007  +80.331 -167.262  -65.223 -121.349 -127.673 +103.311 +100.524
EOF
rot3="$exp2/193-1757/0529+483.vtqbsq"
cat > cfrq3.cf << EOF
chan_ids ABCDEFGH
    8212.99 8252.99 8352.99 8512.99
    8732.99 8852.99 8912.99 8932.99
frqs B~G
* add a channels UV as clones of BE
clone_ids BEUV
clone_snr_chk true
display_chans B~V@2
pc_mode manual
est_pc_manual 1
fringeout_dir `pwd`/xxxx
plot_data_dir xxxx/cfrq3
if station T
 lsb_offset 305.0
* pc_phases_r A~H
* +51.978 +179.505 +157.229 +112.320 -135.190  -33.459 +135.340 +131.803
*  pc_phases_r B~G
*          +179.505 +157.229 +112.320 -135.190  -33.459 +135.340
pc_phases_r BCDEFGUV
*  -5.549 +153.785 +107.841  +85.248  -37.491 +130.697 +174.946 -139.562
   +179.505 +157.229 +112.320 -135.190  -33.459 +135.340 +179.505 -135.190
EOF

for ii in {1..3} ; do [ -d xxxx/cfrq$ii ] || mkdir -p xxxx/cfrq$ii ; done

$verb && dm=-m0 || dm=''

export PARSEDEBUG=1
$verb && echo \
./fourfit $dm   -b AI:X -c cfrq1.cf $exp1
./fourfit $dm   -b AI:X -c cfrq1.cf $exp1 ||
    { echo unable to fourfit $exp1 ; exit 2 ; }
$verb && echo \
./fourfit $dm   -b TV:X -c cfrq2.cf $rot2
./fourfit $dm   -b TV:X -c cfrq2.cf $rot2 ||
    { echo unable to fourfit $exp2 ; exit 3 ; }
$verb && echo \
./fourfit $dm   -b TV:X -c cfrq3.cf $rot3
./fourfit $dm   -b TV:X -c cfrq3.cf $rot3 ||
    { echo unable to fourfit $rot3 ; exit 4 ; }

exit 0
#
# eof
#
