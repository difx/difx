#!/usr/bin/python
"""
Function for overwriting clock parameters in input file with those calculated 
from the log files
"""
import observation
import sys
import getopt
from datetime import datetime
from time import strptime

from pfile import get_parameter, set_parameter
from log2clock import log2delay, clock_parameters
from astro import mjd2datetime

def add_clock(logs, input_file, starttime = None):
    """
    replace clock parameters in input_file
    """
    if starttime == None:
        mjd, startsecs = get_parameter(('START MJD', 'START SECONDS'), input_file)
        mjd = int(mjd)
        mjd += float(startsecs) / 86400.
        starttime = mjd2datetime(mjd)
    l = []
    for i in range(len(logs)):
        intercept, rate = log2delay(logs[i], starttime)
        l += clock_parameters(intercept, rate, i)
    parameters = []
    values = []
    for i in l:
        parameters.append(i[0])
        values.append(i[1])
    set_parameter(parameters, values, input_file)

def main():
    """
    Calculate clock parameters from a list of log files and overwrite existing values in the input file

    Usage:
        log2input.py root "corr1ef.log corr1mc.log corr1ma.log corr1wz.log" [-t --time start_time]

    If the start time is not specified it is taken from the input file

    Start time must be full date and time in format
    20080201T200229
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "t:", ["time="])
    except getopt.GetoptError, err:
        # print help information and exit:
        print err # will print something like "option -a not recognized"
        print main.__doc__
        sys.exit(2)

    if not len(args) == 2:
        print 'Error Wrong number of arguments'
        print main.__doc__
        sys.exit(2)
    input_file = args[0] + '.input'
    logs = args[1].split()
    #defaults
    starttime = None
    #command line options
    for o, a in opts:
        if   o in ("-t", "--time"):
            starttime = datetime(*strptime(a, "%Y%m%dT%H%M%S")[0:6])
    add_clock(logs, input_file, starttime)
if __name__ == "__main__":
    main()
