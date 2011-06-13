#!/bin/csh -f
#
# This script acts as the gateway to HOPS, the Haystack Observatory
# Postprocessing System.  It provides organized and semi-intelligent
# access to the various documentation files found in $PROGDOC or,
# by default, in /usr/local/doc
#
# Written by CJL, April 1994
#################################################################

					# Make sure we have proper setup
env_check vhelp
if ($status != 0) exit (1)
					# Cancel unnecessary aborts
set nonomatch
					# Get to correct directory
if ($?PROGDOC == 1) then
    cd $PROGDOC
else
    cd /usr/local/doc
endif
					# No arguments, grep out synopses
					# and add generic vhelp document
if ($1 == "") then
    echo ""
    echo "vhelp:  Program       Description"
    echo "vhelp:  -------       -----------"
    grep SYNOPSIS *.doc | grep -v template | awk -f vhelp1.awk 
    cat vhelp.doc
    echo ""
					# Program name specified, locate it
else 
    set progname=$1.doc
    if (! -e $progname) then
	set progname=$1*.doc
    endif
    if (${#progname} > 1) then
	echo "ambiguous argument '" $1 "'"
    else if (! -e $progname) then
	echo "No documentation file for " $1
					# No second arg, just more the whole file
    else if ($2 == "") then
	echo ""
	cat $progname
	echo ""
    else if ($3 != "") then
	echo "vhelp has at most two arguments"
					# Subsection requested, go get it
    else
	echo "\nvhelp: Subsection from file" $progname
	echo "vhelp:"
	awk -f vhelp2.awk -v section=$2 $progname
	echo ""
    endif
endif

