#!/usr/bin/python
"""
TODO make write_antenna more like log2clock returning a list rather than printing directly
"""
import sys
import getopt


import observation
from astro import df2hms, datetime2mjd
from readvex import VexSource, VexSched
from pfile import print_parameter
from observation import increment

def printscan(n_scan, start, npoints, source, ra, dec, calcode, obscode, calcfile):
    scan = "SCAN %d " % (n_scan)
    print_parameter(scan + 'POINTS',  str(int(npoints)), calcfile)
    print_parameter(scan + 'START PT', str(int(start)), calcfile)
    print_parameter(scan + 'SRC NAME', obscode + '_default', calcfile)
    print_parameter(scan + 'REAL NAME', source, calcfile)
    print_parameter(scan + 'SRC RA', '%.15f' % ra, calcfile)
    print_parameter(scan + 'SRC DEC', '%.15f' % dec, calcfile)
    print_parameter(scan + 'CALCODE', calcode, calcfile)
    print_parameter(scan + 'QUAL', 0, calcfile)

def write_scan(vex_path, calcfile, offset = None, tail = None, inc = None, obscode = None):
    print 'Writing scan table of calc table'
    if offset == None:
        offset = observation.offset
    if tail == None:
        tail = observation.tail
    if inc == None:
        inc = observation.increment
    if obscode == None:
        obscode = ''
    #get info from vex file
    source_dict = VexSource(vex_path)
    sched = VexSched(vex_path)

    #initialise variables
    n_scan = -1
    laststart = 0
    lastpoints = 0
    thisstart = 0
    thispoints = 0
    thisend = 0
    nextstart = 0
    nextpoints = 0
    nextsource = ''
    nextra = 0
    nextdec = 0
    nextcalcode = '  '

    print_parameter('NUM SCANS', len(sched.keys()), calcfile)

    for scan in sched.keys():
        # This loop goes over every scan, and for each
        # Writing out the *previous* scan:
        # i.e.
        # everything read in current loop is called next*
        # on the next loop this is copied to this*
        # on the next loop this is copied to last*
        laststart = thisstart
        lastend = thisend
        thisstart = nextstart
        thissource = nextsource
        thisra = nextra
        thisdec = nextdec
        thiscalcode = nextcalcode

        #get new info
        time =  sched[scan]['start'][0]
        day = int(datetime2mjd(time))
        nextstart = (day * 86400 + (time.hour * 3600.0) + (time.minute * 60) + time.second) / inc
        nextsource = sched[scan]['source'][0]

        try:
            nextra = source_dict[nextsource]['ra'][0]
            nextdec = source_dict[nextsource]['dec'][0]
        except:
            raise RuntimeError, "unknown source " + nextsource

        # check all the stations have the same duration
        # if not warn and take the longest
        duration = sched[scan]['station'][0][2]
        for i in range(1, len(sched[scan]['station'])):
            if not duration == sched[scan]['station'][i][2]:
                'Warning Strange durations in scan ' + scan
                if duration < sched[scan]['station'][i][2]:
                    duration = sched[scan]['station'][i][2]

        if n_scan == -1:
            first = nextstart
        nextstart -= first

        # nextstart is the start of the current pointing
        # nextstart + nextpoints is the end of the current pointing
        # lastpoint is the start of the last pointing
        # lastpoint + lastpoints is the end of the last pointing

        # we are actually writing lastpoint
        if n_scan >= 0:
        #run on everyloop except the first
            npoints = nextstart - thisstart
            thisstarthms = df2hms((first + thisstart - offset)* inc / 86400.)
            thisendhms = df2hms((first + thisstart - offset + npoints)* inc / 86400.)
            printscan(n_scan, thisstart - offset, npoints, thissource, thisra, thisdec, thiscalcode, obscode, calcfile)
        n_scan += 1

    #print out last scan once we've fallen off the end of the loop
    npoints = duration + tail
    nextstarthms = df2hms((first + nextstart - offset) * inc / 86400.)
    nextendhms =   df2hms((first + nextstart - offset + npoints) * inc / 86400.)

    printscan(n_scan, nextstart - offset, npoints, nextsource, nextra, nextdec, nextcalcode, obscode, calcfile)

def main():
    """
Produce .calc scan table from a vex file.

Usage:
    vex2sched.py root
    
Options:    
-o --offset      offset
-t --tail        tail

-i --increment   increment

By default, each scan starts at the start start time that scan in the vex file 
and finishes at the start time of the next scan

The final scan lasts the duration given in the vex file + tail.

offset makes each scan start offset seconds earlier and finish offset second earlier.

increment is the number of seconds of increment in the calc file
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "o:t:i:", ["offset=", "tail=", "increment="])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if not len(args) == 1:
        print "Error: Wrong number of arguments"
        print main.__doc__
        sys.exit(2)

    # read arguments
    root = args[0]

    #defaults:
    offset = None
    tail = None
    inc = None

    for o, a in opts:
        if   o in ("-o", "--offset"):
            offset = int(a)
        elif o in ("-t", "--tail"):
            tail = int(a)
        elif o in ("-i", "--increment"):
            inc = float(a)

    vex_path = root + '.skd'
    try:
        calcfile = open(root + '.calc', 'w')
    except:
        print "Error opening calc file " + root + '.calc'
        raise

    write_scan(vex_path, calcfile, offset, tail, inc)

if __name__ == '__main__':
    main()
