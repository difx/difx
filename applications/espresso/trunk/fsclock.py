#!/usr/bin/env python
# =======================================================================
# Copyright (C) 2017 Cormac Reynolds
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
#
# extract a clock estimate from an FS log
# Cormac Reynolds: Feb 2017

from __future__ import print_function, division
import optparse
import re
import sys
import numpy
from matplotlib import pyplot
import espressolib


# a priori corrections to the gps log values derived from previous clock
# searches.
global_offsets = {
        "at":43.0, "bd":216.0, "cd":-0.02, "hh":0.65, "ke":-0.03, "mp":-0.16,
        "pa":"0.04", "sv":215.35, "wa":0.0, "ww":-0.10, "yg":0.26, "zc":215.76}

aliases = {}
#        "at": ["atca", "ca"],
#        "hh": ["ht"],
#        "ho": ["hb"]}


def kth_line(x, y, missing_val=None):
    """Estimates a linear regression using the Theil-Sen estimator (otherwise
    known as the Kendall-Theill Robust Line fit).

    Given two sets of data, computes the Kendall-Theill Robust Line fit for all
    pairs of data where both values are non-missing. Assumes that both given
    data sets are the same length.

    :Params x, y:
        The predictor and predictand data, respectively. Each should be a one
        dimensional list.
    :Param missing_val:
        The placeholder for missing values in the dataset.
    :Return:
        A 'Stats' object with the attributes
             y_int, slope - parameters of regression fit to x,y data as formula
             stddev - standard deviation

    """
    assert len(x) == len(y)

    # function to determine whether a value in a pair is invalid
    def valid_pair(pair, missing_val=None):
        """Given a 2-tuple, returns False if either of the values are equal
        to the supplied missing_val.
        """

        return ((pair[0] != missing_val) and (pair[1] != missing_val))

    pairs = zip(x, y)
    # Filter out data pairs where either x_i or y_i equal missing_val
    good_data = [pair for pair in pairs if valid_pair(pair)]
    #good_x, good_y = zip(*good_data)
    good_x, good_y = map(list, zip(*good_data))
    nval = len(good_data)

    # calculate paired slopes, find median value
    if nval > 0:
        nslp = 0
        slopes = []
        for i in range(nval-1):
            for j in range(i, nval):
                if good_x[i] != good_x[j]:
                    nslp += 1
                    slopes.append((good_y[j]-good_y[i])/(good_x[j]-good_x[i]))
        slope = numpy.median(slopes)

    # calculate y-intercept; use original lists so we can get true median
    # for all of the input data
    rx = sorted(good_x)
    ry = sorted(good_y)
    if (nval % 2) == 1:
        imed = nval//2
        x_med = rx[imed]
        y_med = ry[imed]
    else:
        imed = (nval-1)//2
        x_med = (rx[imed]+rx[imed+1])/2.0
        y_med = (ry[imed]+ry[imed+1])/2.0

    y_int = y_med - slope*x_med
    formula = [slope, y_int]

    # calculate standard deviation
    stddev = 0.0
    count = 0
    for piece in x:
        if y[count] is not None:
            stddev = stddev+(y[count]-formula[0]*piece-formula[1])**2
        count = count+1
    stddev = (stddev/nval)**(0.5)

    # return formula and standard deviation
    return formula, stddev


def parse_clock(line, gps_style):
    """Find gps/fmout lines in the log, extract offset and time, convert to
    clock offset with common convention

    Returns time and offset, or None if line did not parse
    Example line:
2018.250.12:44:03.01/fmout-gps/+7.1182E-006
or
2019.192.18:42:11.36#maser#/maser2gps/+7.6755E-006
    """

    date_re = r"(\d{4})\.(\d{3})\.(\d{2}):(\d{2}):(\d{2})"
    clockline = None
    #clockline = re.match(date_re + ".*/(.*fmout.*)/(.*)$", line)
    clockline = re.match(date_re + r".*/(.*gps.*)/(\S*)", line)
    if not clockline:
        return None
    year, doy, hour, minute, sec, clocktype, offset = clockline.groups()
    offset = float(offset)
    if re.match(gps_style+r"[-2]gps", clocktype):
        offset *= -1e6
    elif re.match(r"gps[-2]"+gps_style, clocktype):
        offset *= 1e6
    else:
        return None
    #else:
    #    sys.stdout.write("Clock type not recognised: " + clocktype + "\n")
    #    return None

    vextime = year + "y" + doy + "d" + hour + "h" + minute + "m" + sec + "s"
    time = espressolib.convertdate(vextime)
    return time, offset


def add_aliases(aliases, global_offsets):
    """Add station aliases to global_offsets"""

    for station in aliases:
        for alias in aliases[station]:
            global_offsets[alias] = global_offsets.get(station, 0)
    return global_offsets


def get_station_name(filename):
    """Extract station name from log file name assuming standard convention:
    <expname><2lettercode>.log
    """

    try:
        station = re.search(r"(\w{2}).log", filename).group(1)
        station = station.lower()
    except AttributeError:
        sys.stderr.write("log file name does not follow convention!\n")
        station = "unknown"
    return station


def clockplot(times, offsets, p, filenames):
    """Plot clock values for eyeball check"""

    fit_x = [times[0], times[-1]]
    fit_y = numpy.polyval(p, fit_x)
    pyplot.title(" ".join(filenames), size="small")
    pyplot.xlabel("Time/MJD")
    pyplot.ylabel("GPS Offset/microsec")
    pyplot.plot(times, offsets, ".")
    pyplot.plot(fit_x, fit_y)
    pyplot.show()


# parse the options
usage = """%prog <logfile>
fits to gps/fmout measures in FS log file
"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--noplot", "-P",
        action="store_false", dest="plot", default=True,
        help="Don't plot"
        )
parser.add_option(
        "--no_offset", "-n",
        action="store_false", dest="do_global_offset", default=True,
        help="Don't offset clocks by known a priori station offset"
        )
parser.add_option(
        "--vex", "-v",
        action="store_true", dest="vex", default=False,
        help="Produce VEX format output"
        )
parser.add_option(
        "--station", "-s",
        type="str", dest="station", default=None,
        help="Specify station name (else derive from log filename)"
        )
parser.add_option(
        "--maser", "-m",
        action="store_true", dest="maser", default=False,
        help="Use maser2gps/gps2maser instead of fmout-gps/gps-fmout values"
        )

(options, args) = parser.parse_args()

if len(args) < 1:
    parser.print_help()
    parser.error("Give at least one log file")

if options.maser:
    gps_style = "maser"
else:
    gps_style = "fmout" 

times = []
offsets = []

global_offsets = add_aliases(aliases, global_offsets)

if options.station:
    station = options.station
else:
    station = get_station_name(args[0])

for logfilename in args:
    with open(logfilename) as logfile:
        for line in logfile:
            try:
                clocks = parse_clock(line, gps_style)
            except:
                sys.stderr.write("Could not parse: " + line)
                clocks = None
            if clocks is None:
                continue
            times.append(clocks[0])
            offsets.append(clocks[1])

sort_index = numpy.argsort(times)
times = [times[i] for i in sort_index]
offsets = [offsets[i] for i in sort_index]
if len(times) < 1:
    raise Exception('No valid clock values found')
#p = numpy.polyfit(times, offsets, 1)
p, stddev = kth_line(times, offsets)
offset = numpy.polyval(p, times[0])
rate = p[0]
#print p
#print offset

if options.do_global_offset:
    global_offset = global_offsets.get(station, 0.0)
    sys.stderr.write(
            "A priori offset for {:s}: {:0.2f} usec\n".format(station,
            global_offset))
    offset -= global_offset

v2d_format = """
ANTENNA {station:s} 
{{
  clockOffset = {offset:0.3f}
  clockRate = {rate:0.3E}
  clockEpoch = {epoch:0.3f}
}}
"""

vex_format = """
  def {station:s};
*                 Valid from          clock_early  clock_early_epoch    rate
    clock_early = {epoch_valid:s} : {offset:0.3f} usec : {epoch:s} : {rate:0.3E} ;
  enddef;
"""

epoch_valid = None
if options.vex:
    # clock in usec, rate in sec/sec, dates in vex format, reverse sign
    # convention
    epoch = espressolib.convertdate(times[0], outformat="vex")
    # make valid from 1 day before first gps measure
    epoch_valid = espressolib.convertdate((times[0]-1.0), outformat="vex")
    offset = offset*-1.
    rate = rate/(-24.*3600.*1e6)
    output_format = vex_format
else:
    # clock in microsec, rate in microsec/sec, dates in MJD
    rate = rate/(24.*3600.)
    epoch = times[0]
    output_format = v2d_format

print (output_format.format(
        station=station.upper(), epoch_valid=epoch_valid, epoch=epoch,
        offset=offset, rate=rate))

if options.plot:
    clockplot(times, offsets, p, args)
