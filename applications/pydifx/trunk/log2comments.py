#!/usr/bin/python
"""
Script to parse log file, extracting all comments.
"""
#need to set this up such that graph can be made non-interactively

import sys
import re
from datetime import datetime, timedelta
from time import strptime, strftime

##############################################################################
searchstring = [';"', ':"']
##############################################################################

def get_comments(logfile):
    """
    Get comments from logfile where logfile is an open file object.
    """
    times = []
    comments = []
    #first find out how the file is structured

    logfile.seek(0)
    print 'relevant lines from ' + sys.argv[1]
    for line in logfile:
        for s in searchstring:
            if s in line:
                #n.b. this is ignoring fractional part of the second but that probably doesn't matter
                t = datetime(*strptime(line[:17], "%Y.%j.%H:%M:%S")[0:6])
                us = timedelta(0, 0, int(line[18:20]) * 10000)
                times.append(t + us)
                line = line.strip()
                line = line.split(s)
                comments.append(" ".join(line[1:]))
    return times, comments

def print_comments(times, comments):
    for i in range(len(times)):
        print "!",
        print strftime("%j %H:%M:%S", times[i].utctimetuple()).zfill(12) + ' ',
        print ' ',
        print comments[i]


def main():
    """
    Extract comments from a telescope logfile and print to stdout

    Usage:
        log2comments.py path-to-logfile
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        logfilename = sys.argv[1]
        logfile = open(logfilename, 'r')
    except:
        print main.__doc__
        raise RuntimeError, "can't find logfile"
    times, comments = get_comments(logfile)
    print_comments(times, comments)

if __name__ == "__main__":
    main()
