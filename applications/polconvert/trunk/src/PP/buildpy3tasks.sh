#!/bin/sh
#
# Script to do the buildmytasks dance for Python3
#
# The original invocation (python2) noticed the xml file and
# then did all the work.  The newer version has several steps:
# 1. upgrade the xml to the CASA6 standard
# 2. create a package to be loaded
# 3. generate a task to load the package
#
# https://casadocs.readthedocs.io/en/stable/api/casashell/buildmytasks.html
#
echo building py3 casa task with: "$@" with $# args
[ $# -eq 3 ] || { echo script improperly used; exit 1; }
bmt=$1
xml=$2
mod=$3
cwd=`pwd`

echo step 1, upgrade the XML task file
$bmt --upgrade $xml
mkdir .trash
mv $xml.bak .trash

echo step 2, create the package
mkdir -p $mod/private
cp -p $xml $mod
cp task_polconvert.py $mod/private
cd $mod

echo step 3, build the task
$bmt --module $mod $xml
echo '__name__ = "'$mod'"' > __init__.py
echo '__all__ = [ "polconvert" ]' >> __init__.py
echo 'from .polconvert import polconvert' >> __init__.py

echo all done--trash in `pwd`/.trash

#
# eof
#
