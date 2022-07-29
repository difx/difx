#!/bin/bash

if [[ $# -ne 2 ]] || [[ ! -f $1 ]]; then
	echo "" >&2
	echo "Usage: m6dir2filelist.sh <mk6dir_filelist> <path_to_prepend>" >&2
	echo "" >&2
    echo "Converts a mk6dir filelist file into a DiFX file list by prepending" >&2
    echo "the path to the scan names and discarding the scan length field." >&2
    echo "The output can be used for FUSE/file based DiFX correlation as" >&2
    echo "opposed to the DiFX Mark6 native mode correlation." >&2
    echo "" >&2
    exit 1
fi

while IFS= read -r entry || [ -n "$entry" ]; do
	scan_and_mjds=`echo $entry | cut -f 1-3 -d " "`
    flist_entry="${2}$scan_and_mjds"
    echo $flist_entry
done < $1
