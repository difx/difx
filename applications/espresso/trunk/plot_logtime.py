#!/usr/bin/python
# program to plot elapsed time versus correlator time from DiFX logs.
import sys, os, re, optparse, pprint, urllib, datetime, time
import scipy
from scipy import interpolate
from math import *
import matplotlib
from matplotlib import pyplot
import numpy

usage = '''%prog [options] <difxlog> 
extracts the observation and correlation times from the <difxlog> file and plots the correlation speedup factor'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
parser.add_option( "--output", "-o",
        type='str', dest="outfile", default=False,
        help='print to file' )
parser.add_option( "--time", "-t",
        action="store_true", dest="plottime", default=False,
        help='Plot elapsed time instead of speedup factor' )
parser.add_option( "--averaging_time", "-a",
        type=float, dest="avg", default=300,
        help='Average the data for AVG seconds (300)' )
parser.add_option( "--speedup_poly", "-p",
        type=int, dest="poly_order", default=False,
        help='Fit time data with poly of order p, before differentiating to find speedup factor' )
parser.add_option( "--removegap", "-r",
        action="store_true", dest="removegap", default=False,
        help='Remove gaps in the observation time' )
parser.add_option( "--verbose", "-v",
        action="store_true", dest="verbose", default=False,
        help='Increase verbosity' )

(options, args) = parser.parse_args()
if len(args) < 1:
    parser.print_help()
    parser.error("give a DiFX log file")

filenames = args


title = 'Correlator Speedup ' + str(filenames)
if options.removegap:
    title += ' (no gaps)'
pyplot.title(title)

pyplot.xlabel('Correlation time/sec')
if options.plottime:
    pyplot.ylabel('Observation time/sec')
else:
    pyplot.ylabel('Speedup factor')

no_offset = matplotlib.ticker.ScalarFormatter(useOffset=False)
pyplot.gca().xaxis.set_major_formatter(no_offset)
pyplot.gca().yaxis.set_major_formatter(no_offset)

xdata = numpy.array([0])
ydata = numpy.array([0])
for filename in args:
    thisdatafile = open(filename).readlines()

    this_xdata = []
    this_ydata = []

    #line_label = filename

    nlines = 0
    for line in thisdatafile:
        nlines += 1
        line = line.strip()

        # match the correlator time and observation in the log file
        #obstime_match = re.search(r'(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),(\d{3}).*The approximate mjd/seconds is (.*)', line)
        obstime_match = re.search(r'(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),(\d{3}).*to write out time (.*)', line)
        if not obstime_match:
            continue

        # convert correlator and observation time strings to seconds
        corrtime = obstime_match.groups()[0]
        corr_fracsec = int(obstime_match.groups()[1]) * 1e-3
        #obstime_day, obstime_secs = obstime_match.groups()[2].split('/')
        #obs_secs = int(obstime_day)*(24*60*60.) + int(obstime_secs)
        obs_secs = float(obstime_match.groups()[2])
        corrdatetime = datetime.datetime.strptime(corrtime, '%Y-%m-%d %H:%M:%S')
        corr_secs = time.mktime(corrdatetime.timetuple()) + corr_fracsec

        # this_* are the data extracted for the current log file only
        this_xdata.append(corr_secs)
        this_ydata.append(obs_secs)

    this_xdata = numpy.array(this_xdata)
    this_ydata = numpy.array(this_ydata)

    # find the integration time - typical spacing between mjds in the log
    int_time = numpy.median(numpy.diff(this_ydata))
    if options.verbose:
        print filename, 't_int:', int_time, 'n_int:', len(this_ydata)
    # nskip is fraction of the data points we will keep
    if int_time < options.avg:
        nskip = int(options.avg//int_time)
    else:
        nskip = 1
        sys.stderr.write("Warning: averaging time less than correlator integration time, resetting AVG to: " + str(int_time) + '\n')

    # remove gaps in the observation if requested
    if options.removegap:
        orig_length = this_ydata[-1] - this_ydata[0]
        this_ydata = numpy.arange(0, len(this_ydata)*int_time, int_time)
        print 'removed', orig_length - this_ydata[-1], 'secs'

    # move array to origin (start time=t_int,t_int for first file and continues
    # to next file without a gap)
    this_ydata = this_ydata - this_ydata[0] + ydata[-1] + int_time
    this_xdata = this_xdata - this_xdata[0] + xdata[-1] + int_time

    # only need a fraction of the points (this also effectively smooths)
    this_ydata = [this_ydata[i] for i in range(0, len(this_ydata), nskip)]
    this_xdata = [this_xdata[i] for i in range(0, len(this_xdata), nskip)]

    # concatenate this file's data to the master arrays
    ydata = numpy.concatenate((ydata, this_ydata))
    xdata = numpy.concatenate((xdata, this_xdata))


print 'speedup factor:', ydata[-1]/xdata[-1]

xmax = xdata[-1]
ymax = ydata[-1]

if options.plottime:
    pyplot.plot(xdata, ydata, '.')
else:
    ydata_speedup = ydata/xdata
    pyplot.plot(xdata, ydata_speedup, label='Integrated speedup')
    #time_smooth = interpolate.interp1d(xdata, ydata, kind='cubic')
    #pyplot.plot(xdata, ydata, label=line_label + 'orig')
    #time_smooth = interpolate.splrep(xdata,ydata, s=len(xdata))
    #time_smooth = interpolate.splrep(xdata,ydata)
    #xdata = range(0, int(xdata[-1]), 1)
    #ydata = interpolate.splev(xdata, time_smooth, der=1)
    ydata_diff = numpy.diff(ydata)/numpy.diff(xdata)
    xdata_diff = xdata[0:-1]
    pyplot.plot(xdata_diff, ydata_diff, label='Instantaneous speedup')
    #pyplot.plot(xdata, ydata, label=line_label + 'orig')
    if options.poly_order:
        poly_fit = numpy.polyfit(xdata, ydata, options.poly_order)
        xdata_poly = numpy.linspace(xdata[0], xdata[-1])
        ydata_poly = numpy.polyval(poly_fit, xdata_poly)
        ydata_poly = numpy.diff(ydata_poly)/numpy.diff(xdata_poly)
        xdata_poly = xdata_poly[0:-1]
        pyplot.plot(xdata_poly, ydata_poly, label='Smoothed Instantaneous speedup')

if options.plottime:
    pyplot.plot([0, ymax], [0, ymax], label='Real time')
else:
    pyplot.plot([0, xmax], [1, 1], label='Real time')


pyplot.legend(loc='best', prop={'size':8})


if options.outfile:
    pyplot.savefig(options.outfile)
else:
    pyplot.show()
pyplot.close()


