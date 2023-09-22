#!/usr/bin/python
"""
Script to parse log file, extracting all Tsys.

TODO: write graph functions similar to those of log2clock.py
TODO: change all print statments so that they print to file
TODO: better support for antab files
"""
import sys
from datetime import datetime, timedelta
from time import strptime, strftime

import difxlog as log

##############################################################################
searchstring = '/tsys/'
##############################################################################

def get_tsys(logfile, bandlist = None):
    """
    Get tsys values from logfile where logfile is an open file object.
    """
    times = []
    count = 0
    band = {}
    ifs = []
    #first find out how the file is structured

    logfile.seek(0)
    for line in logfile:
        if searchstring in line:
            t = datetime(*strptime(line[:17], "%Y.%j.%H:%M:%S")[0:6])
            t += timedelta(0, 0, int(line[18:20]) * 10000)
            if len(times) == 0 or not times[-1] == t:
                times.append(t)
                for i in ifs:
                    while not len(band[i]) == len(times) - 1:
                        if len(band[i]) > 0:
                            band[i].append(band[i][-1])
                        else:
                            band[i].append(0.)
                        print 'Warning, error reading lines at time ' + times[-1].isotime()
                        print line.strip()
            count += 1

            l = line.split(',')
            l[0] = l[0].split('/')[-1]
            for i in range(0, len(l), 2):
                if not ifs.count(l[i]):
                    ifs.append(l[i])
                    band[l[i]] = []
                band[l[i]].append(float(l[i+1]))
            continue
    if not bandlist:
        bandlist = ifs
    for b in bandlist:
        print b.center(8),
    print_antab(times, band, bandlist)
    #mygraph(times, band, logfilename )

def print_ifs(ifs, band, bandlist = None):
    """
    Print the ifs (not their values)
    """
    if not bandlist:
        bandlist = band.keys()
    print
    print ''.rjust(12),
    for i in range(len(band[band.keys()[0]])): #long way of saying length of the first dictionary
        for b in ifs:
            print str(band[b][i]).center(8) + ' ',
        print 

def print_antab(times, band, bandlist = None):
    """
    print antab without first line
    """
    if not bandlist:
        bandlist = band.keys()
    else:
        print bandlist
    print
    for i in range(len(band[band.keys()[0]])): #long way of saying length of the first dictionary
        print strftime("%j %H:%M:%S", times[i].utctimetuple()).zfill(12) + ' ',
        for b in bandlist:
            print str(band[b][i]).rjust(8),
        print 

def main():
    """
    Extract Tsys values from the log file and print to stdout

    Usage:
        log2tsys.py path-to-logfile [bandlist]

    where bandlist is a list of IFs:
    e.g.
    log2tsys.py exampleef.log "1u 2u 3u 4u"
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        logfilename = sys.argv[1]
    except IndexError:
        print main.__doc__
    try:
        logfile = open(logfilename, 'r')
    except IOError:
        print "Error: can't find logfile"
        print main.__doc__
        sys.exit(2)
    try:
        bandlist = sys.argv[2]
        bandlist = bandlist.split()
    except IndexError:
        bandlist = None
    get_tsys(logfile, bandlist = bandlist)

if __name__ == "__main__":
    main()
