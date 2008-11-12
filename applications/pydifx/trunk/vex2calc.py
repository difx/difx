#!/usr/bin/python
"""
Creates a full calc file.

Ties together vex2calcheader, vex2calcantenna, vex2sched and eop to create
a full calc file

It is very efficiently in that it reads the vex file multiple times
This may be a problem with very large vex files.

TODO add option to use input2calc.py?
"""
import sys
import getopt
from math import floor

import observation

import vex2calc.vex2calcheader as vex2calcheader
import vex2calc.vex2calcantenna as vex2calcantenna
import vex2calc.vex2sched as vex2sched
import vex2calc.eop as eop
import vex2calc.calcfooter as calcfooter

from astro import datetime2mjd
from vex2calc.readvex import VexSched


def main():
    """
Produce .calc file from a vex file.

Usage:
    vex2calc.py root

n.b. for experiments longer than one day you must
specify the number of days to ensure sufficient eop
parameters
    
Options:    
========
-p  --vexpath           vexpath (root.skd by default)

vex2calcheader.py
    --obscode           obscode (root by default)
    --jobid             jobid
-i  --increment         increment
    --spectralaverage   spectralaverage
    --taperfunction     taperfunction

vex2calcantenna.py
-a  --antennas          antennas

vex2sched.py 
-o  --offset            offset
-t  --tail              tail

eop.py
-e  --extra             extra
-d  --download
-f  --force
-n  --ndays             integer number of days the observation lasts 

For more information on the options, see the docstrings of the individual
scripts in the vex2calc directory
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "p:i:a:o:t:e:dfn:",
                     ["vexpath=",
                      "obscode=", "jobid=", "increment=", "spectralaverage=", "taperfunction=",
                      "antennas=", "offset=", "tail=",  "extra=", "download", "force", "ndays="])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if not len(args) == 1:
        print 'Error: Wrong number of arguments.'

    # read arguments
    root = args[0]

    # set defaults
    vex_path = root + '.skd'
    

    obscode = root
    job_id = None
    increment = None
    spectral_average = None
    taper_function = None
    antennas = None
    offset = None
    tail = None
    extra = None
    download = None
    force = None
    ndays = 0

    # read options
    for o, a in opts:
        if o in ("-p", "--vexpath"):
            vex_path = a
        elif o == "--obscode":
            obscode = a
        elif o == "--jobid":
            job_id = a
        elif o in ("-i", "--increment"):
            increment = int(a)
        elif o == "--spectralaverage":
            spectral_average =  a
        elif o == "--taperfunction":
            taper_function = a
        elif o in ("-a", "--antennas"):
            antennas = a.split()
        elif o in ("-o", "--offset"):
            offset = int(a)
        elif o in ("-t", "--tail"):
            tail = int(a)
        elif o in ("-e", "--extra"):
            extra = int(a)
        elif o in ("-d", "--download"):
            download = True
        elif o in ("-f", "--force"):
            force = True
        elif o in ("-n", "--ndays"):
            ndays = int(a)

    try:
        f = open(vex_path, 'r')
    except:
        "Can't open vex file"
        raise

    f.close()
    calcfilepath = root + '.calc'
    calcfile = open(calcfilepath, 'w')
    
    vex2calcheader.write_header(vex_path, calcfile, obscode, job_id, increment, spectral_average, taper_function)
    vex2calcantenna.write_antenna(vex_path, calcfile, antennas)
    vex2sched.write_scan(vex_path, calcfile, offset, tail, increment)
    eop.write_eops(floor(VexSched(vex_path).startmjd() + 0.5), calcfile, extra, download, force, ndays)
    calcfooter.write_footer(obscode, calcfile)

if __name__ == '__main__':
    main()
