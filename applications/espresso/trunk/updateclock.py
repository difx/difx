#!/usr/bin/env python
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

# simple script to take a v2d file and update the clock.
# The parser is a bit shoddy but will work for our usual v2d files.
# Cormac Reynolds. Original program: May 2010

import sys
import re
import optparse
import time
import os
import espressolib
import numpy


def parseparam(param, line):
    value = re.search(param + r"\s*=\s*(\S+)", line).group(1)
    return value


def updateclock(
        clockepoch, clockoffset, clockrate, newclockepoch, offset_adjust,
        rate_adjust, frequency):
    # offsets are in microsec and rate in microsec/sec

    # residual rate is given in mHz at a frequency in MHz. Must convert to
    # microsec/second
    rate_adjust = rate_adjust*1e-3/frequency

    newclockoffset = clockoffset + offset_adjust + \
        (newclockepoch-clockepoch)*clockrate*(24.*60.*60.)
    newclockrate = clockrate + rate_adjust
    return newclockoffset, newclockrate


# program starts here

# parse the options
usage = """%prog [options] <expname.v2d>
adjusts the clocks in the v2d format file <expname.v2d>"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--offset", "-o",
        type="str", dest="offset_adjust", default=None,
        help="List of clock offset adjustments (microsec)")
parser.add_option(
        "--rate", "-r",
        type="str", dest="rate_adjust", default=None,
        help="List of clock rate adjustments (mHz) (requires frequency)")
parser.add_option(
        "--epoch", "-e",
        type="str", dest="newclockepoch", default=None,
        help="Clock epoch (in MJD or Vex time) for the output file")
parser.add_option(
        "--station", "-s",
        type="str", dest="station", default=None,
        help="Comma separated list of stations")
parser.add_option(
        "--frequency", "-f",
        type="float", dest="frequency", default=None,
        help="Observation frequency (MHz), required for rate calculations")
parser.add_option(
        "--addmean", "-m",
        action="store_true", dest="addmean", default=False,
        help="Add the mean clock offset/rate adjustment")

(options, args) = parser.parse_args()
if len(args) != 1:
    parser.print_help()
    parser.error("no v2d file given")


newclockepoch = options.newclockepoch

if newclockepoch:
    # convert to MJD
    newclockepoch = espressolib.convertdate(newclockepoch, "mjd")

station_list = []
offset_list = []
rate_list = []
if options.station:
    station_list = options.station.split(",")
    station_list = [station.upper() for station in station_list]
if options.offset_adjust:
    offset_list = options.offset_adjust.split(",")
if options.rate_adjust:
    rate_list = options.rate_adjust.split(",")


if options.offset_adjust and len(offset_list) != len(station_list):
    raise Exception("number of stations does not match number of clock offsets")

if options.rate_adjust and len(rate_list) != len(station_list):
    raise Exception("number of stations does not match number of clock rates")

if options.rate_adjust and not options.frequency:
    raise Exception("You must set the frequency (-f) if you are adjusting the rate!")

offset_list = [float(offset) for offset in offset_list]
rate_list = [float(rate) for rate in rate_list]

# offset all adjustments by the mean adjustment so that the net change is 0
if options.addmean:
    if offset_list:
        offset_mean = sum(offset_list)/len(offset_list)
        print "mean offset: {0:0.3f}".format(offset_mean)
    if rate_list:
        rate_mean = sum(rate_list)/len(rate_list)
        print "mean rate: {0:0.3E}".format(rate_mean)
    offset_list = [offset-offset_mean for offset in offset_list]
    rate_list = [rate-rate_mean for rate in rate_list]

station = dict()
for i in range(len(station_list)):
    if not station_list[i] in station:
        station[station_list[i]] = dict()
    try:
        station[station_list[i]]["offset_adjust"] = offset_list[i]
    except:
        station[station_list[i]]["offset_adjust"] = 0
    try:
        station[station_list[i]]["rate_adjust"] = rate_list[i]
    except:
        station[station_list[i]]["rate_adjust"] = 0


if options.frequency:
    frequency = options.frequency
else:
    frequency = 1


#newclockepoch = sys.argv[1]
# read in and strip the v2dfile of whitespace and newlines
v2dfilename = args[0]
v2dfile = open(v2dfilename).readlines()
v2dfile = [v2dfile[i].strip("\n") for i in range(len(v2dfile))]

antname = str()
v2dout = []
do_update = False
cache = []
comment = []
rate_adjust = float()
offset_adjust = float()
for line in v2dfile:
    # extract the comments so we can put them back later
    try:
        comment.append(re.search(r"#.*", line).group(0))
    except:
        comment.append("")

    # remove the comments so we can parse the line. Store the original line in
    # a cache so we can update it at the end of the v2d section if necessary
    line = re.sub("#.*", "", line)
    cache.append(line)

    # find the antenna name
    if "ANTENNA" in line:
        antname = re.search(r"ANTENNA\s+(\w+)", line).group(1).upper()

    # open bracket starts parsing info for this antenna
    if "{" in line and antname:
        rate_adjust = 0
        offset_adjust = 0
        if antname in station:
            rate_adjust = station[antname]["rate_adjust"]
            offset_adjust = station[antname]["offset_adjust"]
            do_update = True
        #if options.newclockepoch:
        #    do_update = True

    # update the clock in the cache if this is the end of the section
    if do_update and "}" in line:
        do_update = False
        oldantname = antname
        antname = str()
        for i in range(len(cache)):
            if "clockRate" in cache[i]:
                clockrate = parseparam("clockRate", cache[i])
                cache[i] = "#" + cache[i]
            if "clockOffset" in cache[i]:
                clockoffset = parseparam("clockOffset", cache[i])
                cache[i] = "#" + cache[i]
            if "clockEpoch" in cache[i]:
                clockepoch = parseparam("clockEpoch", cache[i])
                # convert to MJD if necessary
                clockepoch = espressolib.convertdate(clockepoch, "mjd")
                cache[i] = "#" + cache[i]

        # convert the values to floats.
        try:
            clockoffset = float(clockoffset)
        except:
            raise Exception(
                    "No clockOffset for " + oldantname +
                    " in the .v2d file! Please check format.")
        try:
            clockepoch = float(clockepoch)
        except:
            raise Exception(
                    "No clockEpoch for " + oldantname +
                    " in the .v2d file! Please check format.")
        try:
            clockrate = float(clockrate)
        except:
            raise Exception(
                    "No clockRate for " + oldantname +
                    " in the .v2d file! Please check format.")

        if not newclockepoch:
            newclockepoch = clockepoch

        newclockoffset, newclockrate = updateclock(
                clockepoch, clockoffset, clockrate, newclockepoch,
                offset_adjust, rate_adjust, frequency)

        cachecomment = (
                "# clocks updated on " +
                time.strftime("%Y-%m-%d %H:%M:%S (%z)"))
        offsetline = "  clockOffset = {0:0.3f}".format(newclockoffset)
        rateline = "  clockRate = {0:0.3E}".format(newclockrate)
        epochline = "  clockEpoch = {0:0.3f}".format(newclockepoch)

        cache.insert(-1, cachecomment)
        cache.insert(-1, offsetline)
        cache.insert(-1, rateline)
        cache.insert(-1, epochline)

    # write the new telescope information to the output array
    if not do_update:
        for i in range(len(cache)):
            try:
                cache[i] += comment[i]
            except:
                pass
        v2dout += cache
        cache = []
        comment = []

# backup the original before overwriting
os.rename(v2dfilename, v2dfilename + ".bak")
OUTPUTV2D = open(v2dfilename, "w")

# print the output
for out_line in v2dout:
    print>>OUTPUTV2D, out_line
