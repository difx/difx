#!/usr/bin/python
"""
Script to parse log file, extracting all gps times.

TODO check for gps-fmout or fmout-gps
TODO get graph functions working with a choice between interactive or not
TODO verify results against graphs
TODO add function to check against internet for EVN stations
"""

import sys
import re
import getopt
from datetime import datetime, timedelta
from time import strptime
from math import sqrt
try:
    from numpy.linalg import lstsq
    from numpy import array, zeros
except ImportError:
    print "Error: numpy required"
    raise
    sys.exit(2)

#try:
#    from pylab import plot, axis, savefig, show, plot_date, date2num, figure
#    from matplotlib.dates import date2num, DateFormatter, HourLocator, MinuteLocator
#except ImportError:
#    print 'python-matplotlib must be installed in order to generate plots'

import observation
from pfile import print_parameter


searchstring = 'gps'
gpsline = re.compile(r"""
([0-9]*)\.([0-9]*)\.([0-9]*):([0-9]*):(?:([0-9]+)(\.[0-9]+)?|[0-9]+) / # Time followed by slash
(gps-fmout|fmout-gps) /                                                
[^-+0-9]* # get rid of any character which could not be the start of a float
([-+]?\s*[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?)                         #float
                      """, re.VERBOSE)



def fit(x, y):
    """
    return least squares fit a, b for
    y = ax + b
    """
    a = zeros((len(x), 2), float)
    a[:, 0] = x
    a[:, 1] = 1
    (p, residuals, rank, s) = lstsq(a, y)
    return p


#def mygraph(x, y, fity, filename = None):
#    """
#    plot the tables using matplotlab
#    """
#    x = date2num(x)
#    mformat = DateFormatter("%H:%M")
#    a = HourLocator()
#    b = MinuteLocator(interval=10)
#    fig = figure()
#    ax = fig.add_subplot(111)
#    if filename:
#        ax.set_title('clock offset values from logfile ' + filename)
#    else:
#        ax.set_title('clock offset values from logfile')
#    ax.set_xlabel('Time / hh:mm')
#    ax.set_ylabel('clock offset/ microseconds')
#    ax.xaxis.set_major_locator(a)
#    ax.xaxis.set_major_formatter(mformat)
#    ax.xaxis.set_minor_locator(b)
#    ax.grid(True)
#
#    ax.plot_date(x, y, 'rx')
#    ax.plot_date(x, fity, 'b-')
#    ax.legend(('file values', 'fit'))
#    ax.autoscale_view(tight=True, scalex=True, scaley=True)
#    show()


def log2delay(logfilename, starttime = None, index = 0, graph = False):
    """
    returns a tuple of delay and rate
    """

    if starttime == None:
        starttime = observation.starttime


    offsets = []
    times = []
    count = 0

    try:
        logfile = open(logfilename, 'r')
    except:
        print "Can't open " + logfilename
        raise

    logfile.seek(0)
    for line in logfile:
        if searchstring in line:
            count += 1
            l = gpsline.match(line)
            if l:

                g = l.groups()
                print g
                time = datetime(*strptime(' '.join(g[0:5]), "%Y %j %H %M %S")[0:6])
                if g[5]:
                    time += timedelta(0, float(g[5]), 0)
                offset = float(g[7].replace(' ', ''))
                if g[6] == "fmout-gps":
                    offset *= -1
                times.append(time)
                offsets.append(offset)

    #ok, let's start with the first time and define all others according to it
    if starttime:
        t0 = starttime
    else:
        print 'WARNING: No start time given! Using first time in log file'
        t0 = times[0]
    ts = []
    for t in times:
        td = t - t0
        ts.append(td.days * 86400.0 + td.seconds + (td.microseconds / 1000000.0))
    offsets = array(offsets) * 1000000.0
    ts = array(ts)
    fits = fit(ts, offsets)
    fity = array(ts) * fits[0] + fits[1]
    rate = fits[0]
    intercept = fits[1]
    print logfilename
    print "time[0]   : " + str(times[0])
    print "start time: " + str(t0)
    print "time[-1]  : " + str(times[-1])
    print "Offset 0  : " + str(offsets[0] ).rjust(19) + ' us'
    print "Intercept : " + str(intercept).rjust(19)    + ' us'
    print "Rate      : " + str(rate).rjust(19)    + ' us/s'
    print "Rate      : " + str(rate * 3600).rjust(19)    + ' us/hr'
    print "Standard deviation of points from line"
    print "          :" + str(sqrt(sum((offsets - fity) ** 2)/len(fity))) + ' us'
    print

    return intercept, rate

def clock_parameters(intercept, rate, telescope_index):
    """
    Return clock parameters formatted for an input file
    """
    # need to correct for rounding errors
    intercept = '%.3f' % intercept
    rate = '%.8f' % rate
    telescope_index = str(telescope_index)
    return [['CLOCK DELAY (us) ' + telescope_index, intercept],
           ['CLOCK RATE(us/s) ' + telescope_index, rate]]

def main():
    """
    Generate an input file clock delay and rate and print  to stdout

    Usage:
        log2clock.py path-to-logfile [-t --time start_time], [-i --index telescope_index]

    start time must be full date and time in format
    20080201T200229
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "t:i:", ["time=", "index="])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)

    if not len(args) == 1:
        print 'Error Wrong number of arguments'
        print main.__doc__
        sys.exit(2)

    # read arguments
    logfilename = args[0]

    # set defaults
    inputtime = None
    telescope_index = 0

    for o, a in opts:
        if   o in ("-t", "--time"):
            inputtime = datetime(*strptime(a, "%Y%m%dT%H%M%S")[0:6])
            log.debug('start time: ' + inputtime.isoformat())
        elif o in ("-i", "--index"):
            telescope_index = int(a)
            log.debug('telescope_index: ' + a)

    intercept, rate = log2delay(logfilename, inputtime)
    l = clock_parameters(intercept, rate, telescope_index)
    for line in l:
        print_parameter(line[0], line[1])

if __name__ == "__main__":
    main()
