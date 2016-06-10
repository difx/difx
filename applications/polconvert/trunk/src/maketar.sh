#!/bin/sh
#
# complete set
#

cfiles='
    CalTable.cpp
    CalTable.h
    DataIO.cpp
    DataIO.h
    DataIOFITS.cpp
    DataIOFITS.h
    DataIOSWIN.cpp
    DataIOSWIN.h
    INSTALL
    _PolConvert.cpp
    polconvert.xml
    Weighter.h
    Weighter.cpp
    '

scripts='
    asdmpolconvert.py
    casapolconvert.py
    prepolconvert.py
    maketar.sh
    mytasks.py
    setup.py
    task_polconvert.py
    xy-guess.py
    '

built='
    _PolConvert.so
    polconvert.py
    polconvert_cli.py
    '

version=${1-`date +%Y%m%dT%H%M%S`}

tar zcf PolConvert_$version.tar.gz $cfiles $scripts

#
# eof
#
