#!/usr/bin/env python
# =======================================================================
# Copyright (C) 2018 Cormac Reynolds
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

# Cormac Reynolds, June 2018: program to return ATCA refant at given epoch.


from __future__ import print_function, division
import optparse
import re
import requests
import espressolib


def vexdate2atca(date):
    """Convert date from vex format to ATCA cgi script format"""

    year_index = date.find("y")
    year = date[0:year_index]
    startdate = re.sub("^.*y", "", date)
    startdate = startdate.replace("d", "/")
    startdate = startdate.replace("h", ":")
    startdate = startdate.replace("m", ":")
    startdate = startdate.replace("s", "")

    sec = int(startdate[-2:]) + 1
    enddate = startdate[0:-2] + "{:02d}".format(sec)

    return startdate, enddate, year


usage = """%prog <date> [<date>]...
<date> can either be in MJD, VEX, ISO8601 or VLBA format
Returns ATCA tied array position"""
parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
#parser.add_option(
#        "--vex", "-v",
#        dest="vex", action="store_true", default=False,
#        help="Produce vex format output")

(options, args) = parser.parse_args()

if len(args) < 1:
    parser.print_help()
    parser.error("Give date(s) in MJD, VEX, ISO8601 or VLBA format")

atca_url_template = (
        "http://www.atnf.csiro.au/cgi-bin/vlbi/atca_summary.pl?"
        "start={}&end={}&year={}&site=ATCA")
#print >>sys.stderr, "Fetching ATCA status from", atca_url_template

refant_pad = None
for targetdate in args:
    # convert targetdate to vex
    targetdate = espressolib.convertdate(targetdate, "vex")
    # convert from vex to atca_summary format
    startdate, enddate, year = vexdate2atca(targetdate)

    # fetch atca summary data
    url = atca_url_template.format(startdate, enddate, year)
    atca_summary = requests.get(url).content.decode("utf-8").split("\n")

    # parse the summary page
    refant_pad = None
    pads = []
    for iline, line in enumerate(atca_summary):
        if iline == 1:
            pads = line.split()
        if iline == 2:
            refant = re.sub(r"Refant:\s*CA0", "", line)
            refant = int(refant)
            refant_pad = pads[refant-1]
            print ("{}: {}".format(targetdate, refant_pad))

print ("updatepos.py ATCA AT_{}".format(refant_pad))
