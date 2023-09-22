#!/bin/sh
#!/bin/sh
#
# This script is the sh version of vhelp, suitable for machines
# which do not have csh installed.  It provides organized and
# semi-intelligent access to the various documentation files
# found in $PROGDOC or, by default, in /usr/local/doc
#
# Written by CJL, April 1994
#################################################################
cat=cat
[ -x `type -p less` ] || cat=more
					# Make sure we have proper setup
env_check vhelp
if [ $? -ne 0 ] ; then
    echo "Your environment was not set up--source hops.bash."
    exit 1
fi
					# Cancel unnecessary aborts
shopt -s failglob
					# Get to correct directory
[ -z "$PROGDOC" ] && export PROGDOC=/usr/local/doc
[ -d "$PROGDOC" ] && cd $PROGDOC || {
    echo "Cannot find any documentation--neither of"
    echo "PROGDOC or /usr/local/doc exist"
    exit 2
}
					# No arguments, grep out synopses
					# and add generic vhelp document
if [ "$1" = "" ] ; then
    echo ""
    echo "vhelp:  Program       Description"
    echo "vhelp:  -------       -----------"
    grep SYNOPSIS *.doc |\
    grep -v template | awk -f vhelp1.awk
    $cat vhelp.doc
    echo ""
					# Program name specified, locate it
else 
    progname=$1.doc
    second=$2
    third=$3
    if [ ! -e $progname ] ; then
        set -- $1*.doc no-such-file
	progname="$1"
    fi
    if [ $# -gt 2 ] ; then
	echo "ambiguous argument '" $@ "'"
    elif [ ! -e "$progname" ] ; then
	echo "No documentation file for " $1
					# No 2nd arg, just more the whole file
    elif [ -z "$second" ] ; then
	echo ""
	$cat "$progname"
	echo ""
    elif [ -n "$third" ] ; then
	echo "vhelp has at most two arguments"
					# Subsection requested, go get it
    else
        echo ""
	echo "vhelp: Subsection from file" $progname
	echo "vhelp:"
	awk -f vhelp2.awk -v section=$second $progname
	echo ""
    fi
fi

#
# eof
#
