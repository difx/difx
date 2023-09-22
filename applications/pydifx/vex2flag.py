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

    shrink = timedelta(0, shrink, 0)

    durations = []
    times = []
    #first copy all times into a single array
    for s in sched.values():
        times.append(s['start'][0])
        durations.append(dict(zip([s['station'][i][0] for i in range(len(s['station']))],
                             [s['station'][i][1:3] for i in range(len(s['station']))])))
    # so now we have all the start times in times and all the durations for all
    # the telescopes in durations as a list of dictionaries

    n = len(durations)
    for telid in ids:
        if not printuv:
            print >> flagfile, "# Ant D.O.Y.start D.O.Y.end RecChan reason"
        # we flag the gap between scan[i] and scan[j]
        i = 0
        j = 1
        
        #keep going until i reaches the penultimate scan
        while i < (n - 1):
            if i == 0 and not telid in durations[i].keys():
                # if the telescope's not in the first scan start flagging from
                # the start of the observation.  Correlator starts here so
                # shouldn't be any need to flag before this point.
                startflag = sched.start()
            else:
                while i < (n - 2) and not telid in durations[i].keys():
                    # find a scan which has the relevant telescope in
                    i += 1
                #start flagging from the end of this scan
                startflag = times[i] + timedelta(0, durations[i][telid][1], 0) + shrink
            #start searching for an end point from the next scan onwards
            j = i + 1

            while not telid in durations[j].keys():
                j += 1
                if j == (n - 1):
                    #Antenna not in the last scan. End here.
                    endflag = sched.end()
                    break
            else:
                endflag = times[j] + timedelta(0, durations[j][telid][0], 0) - shrink

            if printuv:
                printuvflag(antennas, iddict[telid], startflag, endflag, starttime,
                            "source change in progress", flagfile)
            else:
                printdifxflag(iddict[telid], startflag, endflag,
                              " -1 'source change in progress'", flagfile)
            i = j #start from last end

def main():
    """
Produce flag table from a vex file, based on time off source.

Usage:
    vex2flag.py root
    
Options:    
-s --shrink       shrink
-u --uvflg
-d --difxflag
-f --flagfilename flag filename
-v --vexfilename  vex filename
Can produce either a input file for the aips task UVFLG or an input file to
difx2fits.

By default the flagging duration lasts for the entire gap between the scans.

shrink will shrink it on either side by shrink seconds
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "s:udf:v:",
                                       ["shrink=", "uvflg", "difxflag", "flagfilename=", "vexfilename="])
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

    # set defaults:
    shrink = None
    printuv = None
    flagfilename = 'flag'
    vex_path = root + '.skd'
    vex_path2 = root + '.vex'
    vex_path_opt = None
    
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
        if o in ("-v", "--vexfilename"):
            vex_path_opt = a

    if vex_path_opt:
        try:
            f = open(vex_path_opt, 'r')
        except:
            "Can't open vex file"
            raise
    try:
        f = open(vex_path, 'r')
    except:
        try: 
            f = open(vex_path2, 'r')
        except:
            "Can't open vex file"
            raise
    try:
        flagfile = open(flagfilename, 'w')
    except:
        print "Error opening flag file " + str(flagfilename)
        raise

    write_flag(vex_path, flagfile, shrink, printuv)

if __name__ == '__main__':
    main()
