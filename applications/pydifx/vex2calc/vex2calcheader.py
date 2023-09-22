#!/usr/bin/python
"""
TODO make write_header more like log2clock returning a list rather than printing directly
"""
import sys
import os
import getopt

import observation
from pfile import print_parameter
from astro import datetime2mjd
from readvex import VexSched


def write_header(vex_path, calcfile, obscode = "", job_id = None, increment = None, spectral_average = None, taper_function = None):
    print "Writing header of .calc file"
    try:
        difx_version = os.environ['DIFX_VERSION']
    except:
        print "Warning, DIFX_VERSION not set. Setting to ''"
        difx_version = ''
    sched = VexSched(vex_path)
    #cross match telescope names against input file in final version?
    i = 0
    if job_id == None:
        job_id = observation.job_id
    if increment == None:
        increment = observation.increment
    if spectral_average == None:
        spectral_average = observation.spectral_average
    if taper_function == None:
        taper_function = observation.taper_function

    start_datetime = sched[sched.keys()[0]]['start'][0]
    start_time = str(datetime2mjd(start_datetime))
    last_scan = datetime2mjd(sched[sched.keys()[-1]]['start'][0])
    end_time = last_scan + float(sched[sched.keys()[-1]]['station'][0][2]) / 86400.
    print_parameter("JOB ID", job_id, calcfile)
    print_parameter("JOB START TIME", start_time, calcfile)
    print_parameter("JOB END TIME",  + end_time, calcfile)
    print_parameter("DUTY CYCLE", 1.0, calcfile)
    print_parameter("OBSCODE", obscode.upper(), calcfile)
    print_parameter("DIFX VERSION", difx_version, calcfile)
    print_parameter("SUBJOB ID", '0', calcfile)
    print_parameter("START MJD", start_time, calcfile)
    print_parameter("START YEAR", start_datetime.year, calcfile)
    print_parameter("START MONTH", start_datetime.month, calcfile)
    print_parameter("START DAY", start_datetime.day, calcfile)
    print_parameter("START HOUR", start_datetime.hour, calcfile)
    print_parameter("START MINUTE", start_datetime.minute, calcfile)
    print_parameter("START SECOND", start_datetime.second, calcfile)
    print_parameter("INCREMENT (SECS)", increment, calcfile)
    print_parameter("SPECTRAL AVG", spectral_average, calcfile)
    print_parameter("TAPER FUNCTION", taper_function, calcfile)

def main():
    """
Extract $SCHED block from vexfile and create .calc file header

Usage:
    readvex.py vex.file

Options:
    --obscode           obscode (root by default)
    --jobid             jobid
-i  --increment         increment
    --spectralaverage   spectralaverage
    --taperfunction     taperfunction

    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "i:",
                     ["obscode=", "jobid=", "increment=", "spectralaverage=", "taperfunction="])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if len(args) > 1:
        print "Error: Wrong number of Arguments."


    # read arguments
    vex_path = args[0]

    # set defaults
    obscode = os.path.splitext(vex_path)[0]
    job_id = observation.job_id
    increment = observation.increment
    spectral_average = observation.spectral_average
    taper_function = observation.taper_function

    # read options
    for o, a in opts:
        if o == "--obscode":
            obscode = a
        if o == "--jobid":
            jobid = a
        if o in ("-i", "--increment"):
            increment = int(a)
        if o == "--spectralaverage":
            spectral_average =  a
        if o == "--taperfunction":
            taper_function = a

    write_header(vex_path, sys.stdout, obscode)

if __name__ == '__main__':
    main()
