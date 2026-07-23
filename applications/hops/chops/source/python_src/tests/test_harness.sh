#!/bin/bash

# use the version from the source tree
TESTDATA_ARCHIVE="/home/gbc-work/HOPS/build-3.26-nd/../trunk/chops/source/python_src/tests/3593.tar.gz"
tarsource=''
if [ -s "$TESTDATA_ARCHIVE" ]; then
    tarsource='from source tree'
elif [ -n "$MHO_REGRESSION_DATA" -a -d "$MHO_REGRESSION_DATA" ]; then
    # try to find it in the MHO area
    unpack="/home/gbc-work/HOPS/build-3.26-nd/../trunk/data/legacy_unpack.sh"
    if [ -x "$unpack" ] ; then
        assignment=`MHO_REGRESSION_EXTRACT=false $unpack 3593`
        [ -n "$assignment" ] && eval "$assignment"
        TESTDATA_ARCHIVE=$mhoregtgz
        if [ -s "$TESTDATA_ARCHIVE" ]; then
            tarsource='from MHO Data'
        fi
    fi
else
    echo not finding data tarball, so disabling check
    RUNCHOPSCHECK='disabled'
    echo you should try to locate a copy and place it in
    echo /home/gbc-work/HOPS/build-3.26-nd/../trunk/chops/source/python_src/tests
    echo or alternatively set up an MHO_REGRESSION_DATA area
fi
if [ -n "$tarsource" ] ; then
    echo using test archive from here:
    ls -l $TESTDATA_ARCHIVE
fi
echo

#default is to run a shortened test
if [ -z "$RUNCHOPSCHECK" ]; then
    #don't run the full suite of tests unless there is an environmental
    #variable called RUNCHOPSCHECK is present, when it is not, we will just
    #check that mk4b library was built correctly

    #run the test
    ################################################################################
    #set up a whole bunch of environmental variables so we can get access to the
    #libraries and python packages which have been built but not yet installed

    OLD_PATH=$PATH
    OLD_LD_LIBRARY_PATH=$LD_LIBRARY_PATH
    OLD_PYTHONPATH=$PYTHONPATH
    OLD_TEXT=$TEXT

    #HOPS not yet setup, so configure these variables
    export PATH=$PATH:"./../../../../postproc/fourfit"
    export LD_LIBRARY_PATH LD_LIBRARY_PATH=$PGPLOT_DIR:$LD_LIBRARY_PATH
    export DEF_CONTROL=/dev/null

    #python packages
    HOPSTESTB_DIR="./../hopstest_module/hopstestb"
    VPAL_DIR="./../vpal_module"

    #python packages with c-libraries
    FFCONTROL_DIR="./../ffcontrol_module"
    FFCONTROL_LIB_DIR="$FFCONTROL_DIR/.libs"

    AFIOB_DIR="./../mk4_module"
    MK4B_DIR="./../mk4_module"
    MK4B_LIB_DIR="$MK4B_DIR/.libs"

    VEXPY_DIR="./../vex_module"
    VEXPY_LIB_DIR="$VEXPY_DIR/.libs"

    #add them to our path
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$FFCONTROL_LIB_DIR:$MK4B_LIB_DIR:$VEXPY_LIB_DIR
    export PYTHONPATH=$PYTHONPATH:$HOPSTESTB_DIR:$VPAL_DIR:$FFCONTROL_DIR:$MK4B_DIR:$AFIOB_DIR:$VEXPY_DIR
    export CHOPS_SRC_DIR="../../../../../trunk/chops"
    export TEXT="../../../../../trunk/chops/source/c_src/vex/text"

    ################################################################################

    #make sure we have permission to mess around in the test/build directory
    #since distcheck plays silly games with where we are allowed to r/w
    chmod -R u+rw "."

    #to avoid making a mess in the source directory its easiest to
    #just copy all the data files we need into the build directory
    CURRENT_TEST_DIR="./3593"
    #starting with a wipe of any previous such directory
    if [ -d "$CURRENT_TEST_DIR" ]; then
        chmod -R u+rw $CURRENT_TEST_DIR
        rm -rf "$CURRENT_TEST_DIR"
    fi
    # this is migrating from its location in HOPS3...3.25
    # TESTDATA_ARCHIVE="../../../../../trunk/chops/source/python_src/tests/3593.tar.gz"
    tar -xzvf "$TESTDATA_ARCHIVE"
    chmod -R u+rw $CURRENT_TEST_DIR

    #run the test suite (environmental var DATADIR should be set before running this)
    #the -B option is makes sure that python doesn't produce any __pycache__ (.pyc)
    #files that we have to clean up later
    /usr/bin/python -B ./test_mk4b.py $CURRENT_TEST_DIR
    MK4B_PASS_FAIL=$?

    #reset
    export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
    export PYTHONPATH=$OLD_PYTHONPATH
    export TEXT=$OLD_TEXT

    #once again make sure we have permission to mess around in the test directory
    chmod -R u+rw ./

    if [ -d "$CURRENT_TEST_DIR" ]; then
      rm -rf "$CURRENT_TEST_DIR"
    fi

    if [ "$MK4B_PASS_FAIL" -eq 0 ]; then
      echo "Partial chops test suite passed."
      exit 0
    else
      echo "Partial chops test suite failed."
      exit 1
    fi

    exit 0
elif [ x$RUNCHOPSCHECK = xdisabled ] ; then
    echo tests disabled
    exit 77
    echo
else
    #run the test
    ################################################################################
    #set up a whole bunch of environmental variables so we can get access to the
    #libraries and python packages which have been built but not yet installed

    OLD_PATH=$PATH
    OLD_LD_LIBRARY_PATH=$LD_LIBRARY_PATH
    OLD_PYTHONPATH=$PYTHONPATH
    OLD_TEXT=$TEXT

    #HOPS not yet setup, so configure these variables
    export PATH=$PATH:"./../../../../postproc/fourfit"
    export LD_LIBRARY_PATH LD_LIBRARY_PATH=$PGPLOT_DIR:$LD_LIBRARY_PATH
    export DEF_CONTROL=/dev/null

    #python packages
    HOPSTESTB_DIR="./../hopstest_module/hopstestb"
    VPAL_DIR="./../vpal_module"

    #python packages with c-libraries
    FFCONTROL_DIR="./../ffcontrol_module"
    FFCONTROL_LIB_DIR="$FFCONTROL_DIR/.libs"

    AFIOB_DIR="./../mk4_module"
    MK4B_DIR="./../mk4_module"
    MK4B_LIB_DIR="$MK4B_DIR/.libs"

    VEXPY_DIR="./../vex_module"
    VEXPY_LIB_DIR="$VEXPY_DIR/.libs"

    #add them to our path
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$FFCONTROL_LIB_DIR:$MK4B_LIB_DIR:$VEXPY_LIB_DIR
    export PYTHONPATH=$PYTHONPATH:$HOPSTESTB_DIR:$VPAL_DIR:$FFCONTROL_DIR:$MK4B_DIR:$AFIOB_DIR:$VEXPY_DIR
    export CHOPS_SRC_DIR="../../../../../trunk/chops"
    export TEXT="../../../../../trunk/chops/source/c_src/vex/text"

    ################################################################################

    #make sure we have permission to mess around in the test/build directory
    #since distcheck plays silly games with where we are allowed to r/w
    chmod -R u+rw "."

    #to avoid making a mess in the source directory its easiest to
    #just copy all the data files we need into the build directory
    CURRENT_TEST_DIR="./3593"
    #starting with a wipe of any previous such directory
    if [ -d "$CURRENT_TEST_DIR" ]; then
        chmod -R u+rw $CURRENT_TEST_DIR
        rm -rf "$CURRENT_TEST_DIR"
    fi
    # this is migrating from its location in HOPS3...3.25
    # TESTDATA_ARCHIVE="../../../../../trunk/chops/source/python_src/tests/3593.tar.gz"
    tar -xzvf "$TESTDATA_ARCHIVE" ./;
    chmod -R u+rw $CURRENT_TEST_DIR

    #run the test suite (environmental var DATADIR should be set before running this)
    #the -B option is makes sure that python doesn't produce any __pycache__ (.pyc)
    #files that we have to clean up later
    /usr/bin/python -B ./test_mk4b.py $CURRENT_TEST_DIR
    MK4B_PASS_FAIL=$?

    /usr/bin/python -B ./test_ffres2pcp.py $CURRENT_TEST_DIR
    FFRES2PCP_PASS_FAIL=$?

    /usr/bin/python -B ./test_fourphase.py $CURRENT_TEST_DIR
    FOURPHASE_PASS_FAIL=$?

    /usr/bin/python -B ./test_pcc_generate.py $CURRENT_TEST_DIR
    PCC_PASS_FAIL=$?

    #reset
    export LD_LIBRARY_PATH=$OLD_LD_LIBRARY_PATH
    export PYTHONPATH=$OLD_PYTHONPATH
    export TEXT=$OLD_TEXT

    #once again make sure we have permission to mess around in the test directory
    chmod -R u+rw ./

    if [ -d "$CURRENT_TEST_DIR" ]; then
      rm -rf "$CURRENT_TEST_DIR"
    fi

    if [ "$FFRES2PCP_PASS_FAIL" -eq 0 -a "$FFRES2PCP_PASS_FAIL" -eq 0 -a  "$FOURPHASE_PASS_FAIL" -eq 0 -a "$PCC_PASS_FAIL" -eq 0 ]; then
      echo "Full chops test suite passed."
      exit 0
    else
      echo "Full chops test suite failed."
      exit 1
    fi
fi

#
# eof
#
