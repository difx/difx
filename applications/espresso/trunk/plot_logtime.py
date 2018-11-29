#!/usr/bin/env python2
# =======================================================================
# Copyright (C) 2016 Cormac Reynolds
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# =======================================================================

# program to plot observation time versus correlator time from DiFX logs.


from __future__ import print_function, division
import sys
import os
import re
import optparse
import datetime
import time
#import scipy
#from scipy import interpolate
from math import *
import matplotlib
import numpy
# Force matplotlib to not use any Xwindows backend if X display not available.
# This doesn't work if there was an X forward but ssh is no longer listening -
# can happen under screen. Deal with that later if initiating the plot fails.
if "DISPLAY" not in os.environ.keys():
    sys.stderr.write("Warning: no display available\n")
    matplotlib.use("Agg")
from matplotlib import pyplot


def corrtime2secs(obstime_match):
    corrtime = obstime_match.groups()[0]
    corr_fracsec = int(obstime_match.groups()[1]) * 1e-3
    corrdatetime = datetime.datetime.strptime(corrtime, "%Y-%m-%d %H:%M:%S")
    corr_secs = time.mktime(corrdatetime.timetuple()) + corr_fracsec
    return corr_secs


usage = """%prog [options] <difxlog>
extracts the observation and correlation times from the <difxlog> file and
plots the correlation speedup factor"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--output", "-o",
        type="str", dest="outfile", default=False,
        help="print to file")
parser.add_option(
        "--time", "-t",
        action="store_true", dest="plottime", default=False,
        help="Plot elapsed correlation time instead of speedup factor")
parser.add_option(
        "--averaging_time", "-a",
        type=float, dest="avg", default=300,
        help="Average the data for AVG seconds (%default)")
parser.add_option(
        "--speedup_poly", "-p",
        type=int, dest="poly_order", default=None,
        help="Fit time data with poly of order p, before differentiating"
        " to find speedup factor")
parser.add_option(
        "--removegap", "-r",
        action="store_true", dest="removegap", default=False,
        help="Remove gaps in the observation time")
parser.add_option(
        "--verbose", "-v",
        action="store_true", dest="verbose", default=False,
        help="Increase verbosity")
parser.add_option(
        "--labelfile", "-l",
        action="store_true", dest="labelfile", default=False,
        help="Label plot with file start positions")
parser.add_option(
        "--grep", "-g",
        type=str, dest="grep", default=False,
        help="Extract only lines containing GREP pattern")

(options, args) = parser.parse_args()
if len(args) < 1:
    parser.print_help()
    parser.error("give a DiFX log file")

filenames = args

title = "Correlator Speedup "  #+ str(filenames)
if options.removegap:
    title += " (no gaps)"

try:
    pyplot.title(title)
except:
    # if setting the title fails, it probably means the default backend has
    # failed because the X session is no longer attached. So switch to a
    # backend that doesn't care (no interactive plots allowed).
    matplotlib.rcParams["backend"] = "Agg"
    pyplot.switch_backend("Agg")
    #print matplotlib.rcParams["backend"]
    sys.stderr.write(
            "Warning: no display available - switching to non-interactive"
            " backend\n")
    pyplot.title(title)

header = " ".join(filenames)
maxnames = 8
if len(filenames) > maxnames:
    header = ", ".join(filenames[0:maxnames-1]) + " ... " + str(filenames[-1])
    header += " (" + str(len(filenames)) + " files)"
pyplot.suptitle(header, fontsize="x-small")

if options.plottime:
    pyplot.xlabel("Correlation time/hours")
    pyplot.ylabel("Observation time/hours")
else:
    pyplot.xlabel("Observation time/hours")
    pyplot.ylabel("Speedup factor")

# remove overall scaling from axis values
no_offset = matplotlib.ticker.ScalarFormatter(useOffset=False)
pyplot.gca().xaxis.set_major_formatter(no_offset)
pyplot.gca().yaxis.set_major_formatter(no_offset)

correlation = numpy.array([0])
observation = numpy.array([0])
new_files = []
starttime = None
for filename in args:
    print ("Processing:", filename)
    thisdatafile = open(filename).readlines()

    this_xdata = []
    this_ydata = []

    for line in thisdatafile:
        line = line.strip()

        # match the correlator time and observation time in the log file
        #obstime_match = re.search(r'(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),(\d{3}).*The approximate mjd/seconds is (.*)', line)
        obstime_match = re.search(
                r"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),(\d{3}).*to write out time (.*)", line)
        if not obstime_match:
            continue
        if (options.grep is not False) and not re.search(options.grep, line):
            continue

        # get the time of the first line in the log file
        if not starttime:
            starttime_match = re.search(
                    r"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}),(\d{3})", line)
            starttime = corrtime2secs(starttime_match)
            this_xdata.append(starttime)
            # give this an observation time of -1 (first vis. is at time 0)
            this_ydata.append(-1)
            continue

        # convert correlator and observation time strings to seconds
        corr_secs = corrtime2secs(obstime_match)
        #obstime_day, obstime_secs = obstime_match.groups()[2].split('/')
        #obs_secs = int(obstime_day)*(24*60*60.) + int(obstime_secs)
        obs_secs = float(obstime_match.groups()[2])

        # this_* are the data extracted for the current log file only
        this_xdata.append(corr_secs)
        this_ydata.append(obs_secs)

    this_xdata = numpy.array(this_xdata)
    this_ydata = numpy.array(this_ydata)

    # if empty file go to next
    if len(this_xdata) < 1:
        print ("Warning:", filename, "has no data")
        continue

    # find the integration time - typical spacing between observation times in
    # the log
    int_time = numpy.median(numpy.diff(this_ydata))
    # nskip is fraction of the data points we will keep
    if int_time < options.avg:
        nskip = int(options.avg//int_time)
    else:
        nskip = 1
        sys.stderr.write(
                filename + "Warning: reducing AVG to: " + str(int_time) + "\n")

    # make sure averaging time is not too long
    #nskip = int(min(nskip, len(this_ydata//2) ))
    minpoints = 3
    if nskip > len(this_ydata)//minpoints:
        nskip = max(len(this_ydata)//minpoints, 1)
        print (filename, ": reducing averaging time to", int_time*nskip)

    # remove gaps in the observation if requested
    if options.removegap:
        orig_length = this_ydata[-1] - this_ydata[0]
        this_ydata = numpy.arange(0, len(this_ydata)*int_time, int_time)
        print (filename, ": removed", orig_length - this_ydata[-1], "secs")

    # move array to origin (start time=int_time for first file and
    # continues to next file without a gap)
    offset_y = observation[-1] + int_time
    offset_x = correlation[-1] + int_time
    this_ydata = this_ydata - this_ydata[0] + offset_y
    this_xdata = this_xdata - this_xdata[0] + offset_x

    if options.verbose:
        print (
                filename, "t_int:", int_time, "n_int:", len(this_ydata),
                "start:", this_ydata[0])

    # only keep a fraction of the points (this also effectively smooths)
    this_ydata = [this_ydata[i] for i in range(nskip, len(this_ydata), nskip)]
    this_xdata = [this_xdata[i] for i in range(nskip, len(this_xdata), nskip)]

    # concatenate this file's data to the master arrays
    observation = numpy.concatenate((observation, this_ydata))
    correlation = numpy.concatenate((correlation, this_xdata))

    if options.labelfile:
        new_files.append((this_xdata[0]/3600., this_ydata[0]/3600.))


# convert elapsed times from seconds to hours
observation /= 3600.
correlation /= 3600.

print ("Observation time: {0:0.3f} hours".format(
        (observation[-1] - observation[0])))
print ("Correlation time: {0:0.3f} hours".format(
        (correlation[-1] - correlation[0])))
print ("Speedup factor  : {0:0.3f}".format(observation[-1]/correlation[-1]))

speedup = []
if options.plottime:
    # simply plot correlation time against observation time
    pyplot.plot(correlation, observation, ".")
else:
    # plot the cumulative speedup and instantaneous speedup against
    # observation time
    speedup = observation[1:]/correlation[1:]
    label1 = "Integrated speedup {speedup:0.3f}".format(speedup=speedup[-1])
    pyplot.plot(observation[1:], speedup, label=label1)
    speedup = numpy.diff(observation)/numpy.diff(correlation)
    xdata_diff = observation[1:]
    pyplot.plot(xdata_diff, speedup, label="Instantaneous speedup")
    if options.poly_order is not None:
        # slightly silly option to fit the data with a polynomial before
        # calculating speedup
        poly_fit = numpy.polyfit(correlation, observation, options.poly_order)
        ydata_poly = numpy.polyval(poly_fit, correlation)
        ydata_poly = numpy.diff(ydata_poly)/numpy.diff(correlation)
        pyplot.plot(
                xdata_diff, ydata_poly, label="Smoothed Instantaneous speedup")

#corrmax = correlation[-1]
#obsmax = observation[-1]

# Plot a line indicating where real-time correlation would be.
xmin = False
xmax = False
ymin = False
ymax = False
if options.plottime:
    xmin = correlation[0]
    xmax = correlation[-1]
    ymin = observation[0]
    ymax = observation[-1]
    pyplot.plot([0, ymax], [0, ymax], label="Real time")
else:
    xmin = observation[0]
    xmax = observation[-1]
    ymin = min(min(speedup), 1)
    ymax = max(max(speedup), 1)
    pyplot.plot([0, xmax], [1, 1], label="Real time")

deltax = xmax - xmin
deltay = ymax - ymin


# annotate plot with the start times of the different files if requested
if options.labelfile:
    textside = -1
    ifile = -1
    for new_file in new_files:
        textside *= -1
        ifile += 1
        pointpos = []
        if options.plottime:
            pointpos = new_file
        else:
            speedup = new_file[1]/new_file[0]
            pointpos = (new_file[1], speedup)
        ytextoffset = (
                len(filenames)-ifile)*0.45*deltay*textside/(len(filenames))
        ytextpos = 0.5*(ymin+ymax) + ytextoffset
        if abs(ytextpos - pointpos[1]) < deltay*0.1:
            ytextpos = 0.5*(ymin+ymax) + ytextoffset*-1
        textpos = (pointpos[0], ytextpos)
        #textpos = (pointpos[0], 0.5*(ymin+ymax) + 0.45*deltay*textside)
        pyplot.annotate(
                filenames[ifile], pointpos, textpos,
                arrowprops=dict(arrowstyle="->"), fontsize="x-small")

pyplot.legend(loc="best", prop={"size": "small"})

#pyplot.figtext(0,0, str(filenames), transform=pyplot.gca().transAxes)

if options.outfile:
    pyplot.savefig(options.outfile)
else:
    pyplot.show()
pyplot.close()
