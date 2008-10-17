#!/usr/bin/python
"""
Functions to create flag files based on the time on source in Vex files

The time on source for each antenna is treated seperately

Functions exist for creating input files both for difx2fits and for
the aips task UVFLG
"""
import sys
import getopt
from datetime import timedelta

import observation
from astro import hms2df
from vex2calc.readvex import VexSched, VexSite

def flagtime(dt):
    """
    Return string of datetime compatible with difx2fits input files.
    """
    return dt.strftime('%j') +\
           ('%.8f' % (hms2df(dt.hour, dt.minute,
                             dt.second + (dt.microsecond / 1000000.))))[1:8]
    
def printdifxflag(antenna, start, end, flag, flagfile):
    """
    Return a line of a flagfile
    
    antenna is the antenna name

    start and end are the start and end times as datetime object

    flag is the string which follows

    flagfile is the open flag file
    """
    print >> flagfile, antenna, flagtime(start), flagtime(end), flag
    
def printuvflag(antenna_list, antenna, start, end, startday, flag, flagfile):
    """
    Return a line of a flagfile
    
    antenna is the antenna name

    antenna list is a list/tuple containing the list of antennas in order

    start and end are the start and end times as datetime object

    startday, is a datetime on the same day as the start of the observation

    flag is the reason

    flagfile is the open flag file
    """
    startdays = start.toordinal() - startday.toordinal()
    enddays = end.toordinal() - startday.toordinal()

    antenna = "ANTENNA=%02d" % (antenna_list.index(antenna) + 1)
    timerange =  "TIMERANGE=%d,%02d,%02d,%02d,%d,%02d,%02d,%02d" %\
                 (startdays, start.hour, start.minute, start.second,
                  enddays, end.hour, end.minute, end.second)
    reason = "REASON='" + flag + "'"
    print >> flagfile, antenna, timerange, reason, '/'


def write_flag(vex_path, flagfile = None, shrink = None, printuv = None):
    if shrink == None:
        shrink = observation.flag_shrink
    if printuv == None:
        printuv = observation.flag_printuv
    if flagfile == None:
        flagfile = observation.flag_flagfilename
    #get info from vex file
    sched = VexSched(vex_path)
    site = VexSite(vex_path)
    iddict = site.id_dict()
    namedict = site.name_dict()
    antennas = site.keys()
    starttime = sched.values()[0]['start'][0]
    ids = [namedict[i] for i in antennas]

    s, us = divmod(shrink * 1000000., 1000000.)
    shrink = timedelta(0, s, us)

    durations = []
    times = []
    for s in sched.values():
        times.append(s['start'][0])
        try:
            durations.append(dict(zip([s['station'][i][0] for i in range(len(s['station']))],
                                      [s['station'][i][1:3] for i in range(len(s['station']))])))
        except:
            print "Error in scan time " + str(times[-1].isoformat())
            raise

    for telid in ids:
        if not printuv:
            print >> flagfile, "# Ant D.O.Y.start D.O.Y.end RecChan reason"
        for i in range(len(durations) - 1):
            if telid in durations[i].keys() and telid in durations[i + 1].keys():
                startflag = times[i] + timedelta(0, durations[i][telid][1], 0) + shrink
                endflag = times[i + 1] + timedelta(0, durations[i + 1][telid][0], 0) + shrink
                if printuv:
                    printuvflag(antennas, iddict[telid], startflag, endflag, starttime,
                                "source change in progress", flagfile)
                else:
                    printdifxflag(iddict[telid], startflag, endflag,
                                  " -1 'source change in progress'", flagfile)

def main():
    """
Produce flag table from a vex file, based on time off source.

Usage:
    vex2flag.py root
    
Options:    
-s --shrink       shrink
-u --uvflg
-d --difxflag
-f --flagfilename flagfilename

Can produce either a input file for the aips task UVFLG or an input file to
difx2fits.

By default the flagging duration lasts for the entire gap between the scans.

shrink will shrink it on either side by shrink seconds
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "s:udf:",
                                       ["shrink=", "uvflg", "difxflag", "flagfilename="])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if not len(args) == 1:
        print "Error: Wrong number of arguments"
        print main.__doc__
        sys.exit(2)

    # set defaults:
    shrink = None
    printuv = None
    flagfilename = 'flag'

    # read arguments
    root = args[0]
    
    #read options
    for o, a in opts:
        if o in ("-s", "--shrink"):
            shrink = float(a)
        if o in ("-u", "--uvflg"):
            printuv = True
            flagfilename = root + '.uvflg'
        if o in ("-d", "difxflag"):
            if printuv == None:
                printuv = False
                flagfilename = 'flag'
            else:
                raise RuntimeError, "flagtype must be either uvflg OR difxflag"
        if o in ("-f", "--flagfilename"):
            flagfilename = a

    vex_path = root + '.skd'
    try:
        flagfile = open(flagfilename, 'w')
    except:
        print "Error opening flag file " + str(flagfilename)
        raise

    write_flag(vex_path, flagfile, shrink, printuv)

if __name__ == '__main__':
    main()
