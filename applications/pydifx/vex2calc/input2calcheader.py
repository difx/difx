#!/usr/bin/python
"""
Generate calc header from input file
"""
import os
import sys
import getopt

import observation
from astro import df2hms, mjd2ymd
from pfile import get_parameter, print_parameter

def calc_gen(input_file, obscode, job_id = None, increment = None, spectral_average = None, taper_function = None):
    """
    generates the first part of a calc file
    """
    print 'writing calc header from input file'
    if job_id == None:
        job_id = observation.job_id
    if increment == None:
        increment = observation.increment
    if spectral_average == None:
        spectral_average = observation.spectral_average
    if taper_function == None:
        taper_function = observation.taper_function

    execute_time, start_mjd, start_seconds, telescope_entries =\
        get_parameter(('EXECUTE TIME (SEC)',
                       'START MJD',
                       'START SECONDS',
                       'TELESCOPE ENTRIES'), input_file)
    
    start_fraction = start_seconds/86400.
    job_start_time = str(start_mjd + start_fraction)
    job_end_time = str(start_mjd + start_fraction + (execute_time/86400.))
    start_year, start_month, start_day  = mjd2ymd(start_mjd)
    start_hour, start_minute, start_second = df2hms(start_fraction)

    # t_names = [('TELESCOPE NAME ' + str(i)) for i in range(int(telescope_entries))]
    # telescope_names = get_parameter(t_names)

    return [['JOB ID',                    job_id],
            ['JOB START TIME',            str(job_start_time)],
            ['JOB END TIME',              str(job_end_time)],
            ['OBSCODE',                   obscode],
            ['START MJD',                 str(start_mjd)],
            ['START YEAR',                str(start_year)],
            ['START MONTH',               str(start_month)],
            ['START DAY',                 str(start_day)],
            ['START HOUR',                str(start_hour)],
            ['START MINUTE',              str(start_minute)],
            ['START SECOND',              str(start_second)],
            ['INCREMENT (SECS)',          increment],
            ['SPECTRAL AVG',              spectral_average],
            ['TAPER FUNCTION',            taper_function]]

def main():
    """
Generate a calc header from an input file

Usage:
    input2calcheader.py root

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
    if len(sys.argv) > 2:
        print "Error: Wrong number of Arguments."

    # read arguments
    root = args[0]
    inputfilepath = root + '.input'

    # set defaults
    obscode = root
    job_id = None
    increment = None     
    spectral_average = None
    taper_function = None
    calcfile = None

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

    for i in calc_gen(inputfilepath, obscode, job_id, increment, spectral_average, taper_function):
        print_parameter(i[0], i[1])

if __name__ == "__main__":
    main()
