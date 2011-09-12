#!/bin/bash +x

# This script performs needed modification to convert a script to use the USNO hardware
#
# Usage: usnomod.sh <project code> <station code>
#		<project code> - observation project code, e.g. N1233
#		<station code> - MK or PT
#
if [ "x" == "x"${1} ] ; then
  echo "Run script with project code as parameter."
  echo "${0} <project code> <station code>"
  exit 1
fi
if [ "x" == "x"${2} ] ; then
  echo "Run script with project code as parameter."
  echo "${0} <project code> <station code>"
  exit 2
fi
cat ${1}.${2}.py \
  | sed s/recorder0\ =\ Mark5C\(\'-1\'\)/recorder0\ =\ Mark5C\(\'usno\'\)/ \
  | sed s/dbe0\ =\ RDBE\(0,\ \'pfb\'\)/dbe0\ =\ RDBE\(2,\ \'pfb\'\)/       \
  | sed s/subarray.set4x4Switch\(\'1A\'/subarray.set4x4Switch\(\'2A\'/     \
  | sed s/subarray.set4x4Switch\(\'1B\'/subarray.set4x4Switch\(\'2B\'/     \
  > ${1}.${2}.py
