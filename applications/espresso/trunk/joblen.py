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

# simple program to parse the .joblist file and return the job length

import optparse
import espressolib

usage = """%prog <joblist>
    will return the length of the jobs in the <joblist> file.
"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")

(options, args) = parser.parse_args()
if len(args) < 1:
    parser.print_help()
    parser.error("Give at least one .joblist file")

jobfilenames = args
line_format = "{:s}: {:s} \t {:0.3f} {:s} \t {:0.2f} {:s} \t {:d} {:s} {:s}"
summary_format = "{:s}: {:0.3f} {:s} \t {:0.2f} {:s} \t {:0.1f} {:s}"

for jobfilename in jobfilenames:
    jobfile = open(jobfilename).readlines()
    header = jobfile.pop(0)
    print header
    passlen = 0
    pass_size = 0
    pass_stations = 0
    for line in jobfile:
        jobinfo = line.split()
        jobstart = espressolib.convertdate(float(jobinfo[1]), outformat="vex")
        joblen = float(jobinfo[2]) - float(jobinfo[1])
        jobstations = int(jobinfo[3])
        jobsize = float(jobinfo[6])
        station_list = " ".join(jobinfo[8:])

        pass_stations = (
                (jobstations*joblen + pass_stations*passlen)/(joblen+passlen))
        passlen += joblen
        pass_size += jobsize

        print line_format.format(
                jobinfo[0], jobstart, joblen*24., "hours", jobsize, "TOps",
                jobstations, "stations", station_list)

    print  summary_format.format(
            "Total", passlen*24., "hours", pass_size, "TOps", pass_stations,
            "stations (avg)")
