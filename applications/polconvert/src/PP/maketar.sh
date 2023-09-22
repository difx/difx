#!/bin/sh
#
# complete set
# 
# version bumps are synched in these files (relative to src)
#   ../Changelog
#   Changelog
#   polconvert.xml
#   task_polconvert.py
#   PP/runpolconvert.py
#

cfiles='
    Changelog
    CalTable.cpp
    CalTable.h
    DataIO.cpp
    DataIO.h
    DataIOFITS.cpp
    DataIOFITS.h
    DataIOSWIN.cpp
    DataIOSWIN.h
    INSTALL
    Makefile.am
    _getAntInfo.cpp
    _PolConvert.cpp
    _PolGainSolve.cpp
    polconvert.xml
    Weighter.h
    Weighter.cpp
    '
scripts='
    QA2/PolConvert_EVN_1.4.py
    PP/drivepolconvert.py
    PP/prepolconvert.py
    PP/runpolconvert.py
    PP/maketar.sh
    PP/README.POLCONVERT
    setup.py
    task_polconvert.py
    '

built='
    _PolConvert.so
    _PolGainSolve.so
    polconvert.py
    polconvert_cli.py
    '

version=${1-`date +%Y%m%dT%H%M%S`}

tar zcf PolConvert_$version.tar.gz $cfiles $scripts

#
# eof
#
