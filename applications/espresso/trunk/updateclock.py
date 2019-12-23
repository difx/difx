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

# simple script to take a v2d file and update the clock.
# The parser is a bit shoddy but will work for our usual v2d files.
# Cormac Reynolds. Original program: May 2010


from __future__ import print_function, division
import re
import optparse
import time
import os
import espressolib


def parseparam(param, line):
    """extract val from keyword=val pair"""

    value = re.search(param + r"\s*=\s*(\S+)", line).group(1)
    return value


def updateclock(
        clockepoch, clockoffset, clockrate, newclockepoch, offset_adjust,
        rate_adjust, frequency):
    """offsets are in microsec and rate in microsec/sec"""

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
adjusts the clocks in the v2d format file <expname.v2d>
E.g.:
updateclock.py -o "AT=0.214,HO=0.151" -r "AT=2.1,HO=0.5" -e 2018y039d04h00m00s -f 8409 v252bl.v2d
*or* equivalently:
updateclock.py -s "AT,HO" -o "0.214,0.151" -r "2.1,0.5" -e 2018y039d04h00m00s -f 8409 v252bl.v2d
"""

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
        "--stations", "-s",
        type="str", dest="stations", default=None,
        help="Comma separated list of stations")
parser.add_option(
        "--frequency", "-f",
        type="float", dest="frequency", default=None,
        help="Observation frequency (MHz), required for rate calculations")
parser.add_option(
        "--addmean", "-m",
        action="store_true", dest="addmean", default=False,
        help="Add the mean clock offset/rate adjustment")
parser.add_option(
        "--global", "-g",
        type="float", dest="global_offset", default=0.0,
        help="Subtract specified delay from all stations")

(options, args) = parser.parse_args()
if len(args) != 1:
    parser.print_help()
    parser.error("no v2d file given")

if options.rate_adjust and not options.frequency:
    raise Exception(
            "You must set the frequency (-f) if you are adjusting the rate!")

newclockepoch = options.newclockepoch

if newclockepoch:
    # convert to MJD
    newclockepoch = espressolib.convertdate(newclockepoch, "mjd")

station_list = []
offset_list = []
rate_list = []
if options.stations:
    station_list = re.sub(r"\s*", "", options.stations)
    station_list = station_list.split(",")
    station_list = [station.upper() for station in station_list]
if options.offset_adjust:
    offset_list = options.offset_adjust.split(",")
if options.rate_adjust:
    rate_list = options.rate_adjust.split(",")

if options.stations:
    if options.offset_adjust and len(offset_list) != len(station_list):
        raise Exception(
                "number of stations does not match number of clock offsets")
    if options.rate_adjust and len(rate_list) != len(station_list):
        raise Exception(
                "number of stations does not match number of clock rates")

stations = dict()
if options.offset_adjust:
    for i, value in enumerate(offset_list):
        station = None
        offset = None
        if station_list:
            station = station_list[i]
            offset = offset_list[i]
        else:
            station, offset = value.split("=")
        if station not in stations:
            stations[station] = dict()
        stations[station]["offset_adjust"] = float(offset) - options.global_offset
        stations[station]["rate_adjust"] = 0

if options.rate_adjust:
    for i, value in enumerate(rate_list):
        station = None
        rate = None
        if station_list:
            station = station_list[i]
            rate = rate_list[i]
        else:
            station, rate = value.split("=")
        if station not in stations:
            stations[station] = dict()
            stations[station]["offset_adjust"] = 0
        stations[station]["rate_adjust"] = float(rate)

if options.addmean:
    # offset all adjustments by the mean adjustment so that the net change is 0
    offset_mean = sum(
            [stations[station]["offset_adjust"] for station in stations])
    offset_mean /= len(stations)
    print ("mean offset: {0:0.3f}".format(offset_mean))
    rate_mean = sum(
            [stations[station]["rate_adjust"] for station in stations])
    rate_mean /= len(stations)
    print ("mean rate: {0:0.3E}".format(rate_mean))

    for station in stations:
        stations[station]["offset_adjust"] -= offset_mean
        stations[station]["rate_adjust"] -= rate_mean

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
        if antname in stations:
            rate_adjust = stations[antname].get("rate_adjust", 0)
            offset_adjust = stations[antname].get("offset_adjust", 0)
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
    OUTPUTV2D.write(out_line + "\n")
