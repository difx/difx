#!/bin/bash

#define the the CHOPS and HOPS source directories
export CHOPS_SRC_DIR="/swc/pops/trunk/chops"
export HOPS_SRC_DIR="/swc/pops/trunk"

#now run all of the import scripts with the checksum option
source ./hops_checksum.sh
retval=$?
echo "retval = $retval"

if [ "$retval" -eq "0" ]
then
  echo "CHOPS and HOPS sources match."
  exit 0
else
  echo "CHOPS and HOPS sources have diverged."
  echo "If that is correct, go to $CHOPS_SRC_DIR"
  echo "and execute import_hops.sh"
  exit 1
fi
