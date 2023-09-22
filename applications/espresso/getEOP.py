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

# Cormac Reynolds, December 2011: program to fetch 5 days of EOPs around the
# given date and return in format suitable for appending to .v2d or .vex file.
# Takes dates in MJD, VEX, ISO8601 or VLBA format.
# October 2017: change to use requests module. Optional vex output.
# May 2019: New download site.
# Sep 2020: Use encrypted ftp.
#
# EOP/ut1 data from:
# ftp://gdc.cddis.eosdis.nasa.gov/vlbi/gsfc/ancillary/solve_apriori/
#

from __future__ import print_function, division
import sys
import time
import os
import optparse
#import requests
import ftplib
import espressolib
#try:
#    import urllib2
#except:
#    sys.stderr.write(
#            "urllib2 module not available, you must use local (-l) mode\n")


def get_leapsec(leapsec_page, target_jd):
    """parse the leap seconds page"""

    tai_utc = None
    for line in leapsec_page:
        linedate = float(line[17:27])
        if linedate > target_jd:
            break
        else:
            tai_utc = int(float(line[38:49]))
    return tai_utc


def ftps_get(url, directory, filename):
    """Return contents of a file on an ftp-ssl site"""
    contents = []
    ftps = ftplib.FTP_TLS(url)
    # login and encrypt connection
    ftps.login()
    ftps.prot_p()
    ftps.cwd(directory)
    ftps.retrlines("RETR {:s}".format(filename), contents.append)

    return contents


usage = """%prog <date>
<date> can either be in MJD, VEX, ISO8601 or VLBA format
Returns 5 days of EOPs around <date> in .v2d format"""
parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--vex", "-v",
        dest="vex", action="store_true", default=False,
        help="Produce vex format output")
parser.add_option(
        "--local", "-l",
        dest="local", action="store_true", default=False,
        help="Take EOPs from local $DIFX_EOPS and $DIFX_UT1LS files instead of"
        " web")
#parser.add_option(
#        "--noverify",
#        dest="verify", action="store_false", default=True,
#        help="Disable HTTPS certificate checking (at own risk!)")

(options, args) = parser.parse_args()

if len(args) != 1:
    parser.print_help()
    parser.error("Give a date in MJD or VEX format")

if options.vex:
    comment = "*"
else:
    comment = "#"

mjd_jd = 2400000.5
# get target MJD
target_mjd = args[0]
# convert to MJD if necessary
target_mjd = espressolib.convertdate(target_mjd, "mjd")
target_jd = round(target_mjd) + mjd_jd


# dates before June 1979 not valid (earliest EOPs)
if (target_jd < 2444055.5):
    raise Exception("Date too early. No valid EOPs before July 1979")

# fetch EOP data
leapsec_page = None
if not options.local:
    #gsfc_url = "ftp://cddis.gsfc.nasa.gov/vlbi/gsfc/ancillary/solve_apriori/"
    #eop_url = gsfc_url + "usno_finals.erp"
    #leapsec_url = gsfc_url + "ut1ls.dat"
    gsfc_url = "gdc.cddis.eosdis.nasa.gov"
    eop_dir = "vlbi/gsfc/ancillary/solve_apriori/"
    eop_filename = "usno_finals.erp"
    leapsec_filename = "ut1ls.dat"

    sys.stderr.write("Fetching EOP data from {:s}\n".format(gsfc_url))
    #eop_page = requests.get(eop_url, verify=options.verify).content.split("\n")
    #eop_page = urllib2.urlopen(eop_url).readlines()
    eop_page = ftps_get(gsfc_url, eop_dir, eop_filename)

    sys.stderr.write(
            "Fetching leap second data from {:s}\n".format(gsfc_url))
    #leapsec_page = requests.get(
    #       leapsec_url, verify=options.verify).content.split("\n")
    #leapsec_page = urllib2.urlopen(leapsec_url).readlines()
    leapsec_page = ftps_get(gsfc_url, eop_dir, leapsec_filename)
    print ("{:s} EOPs downloaded at {:s}".format(
            comment, time.strftime("%Y-%m-%d %H:%M:%S (%z)")))
else:
    # or read from local files
    eop_filename = os.environ.get("DIFX_EOPS")
    assert eop_filename is not None, "You must set $DIFX_EOPS"
    sys.stderr.write("Reading EOP data from {:s}\n".format(eop_filename))
    eop_page = open(eop_filename).readlines()
    eop_page_stats = os.stat(eop_filename)
    eop_update_time = time.gmtime(os.stat(eop_filename)[9])
    leapsec_filename = os.environ.get("DIFX_UT1LS")
    assert leapsec_filename is not None, "You must set $DIFX_UT1LS"
    sys.stderr.write(
            "Reading leap second data from {:s}\n".format(leapsec_filename))
    leapsec_page = open(leapsec_filename).readlines()
    print (
            "{:s} EOPS from {:s} last updated at {:s}".format(
            comment, eop_filename,
            time.strftime("%Y-%m-%d %H:%M:%S (%z)", eop_update_time)))

v2deop = "EOP {0:d} {{ xPole={1:f} yPole={2:f} tai_utc={3:d} ut1_utc={4:f} }}"

vexeop = """def EOP{5:d};
  TAI-UTC = {3:d};
  A1-TAI = 0 sec;
  eop_ref_epoch = {0:s};
  num_eop_points = 1;
  eop_interval = 24 hr;
  ut1-utc = {4:0.6f};
  x_wobble = {1:0.6f} asec;
  y_wobble = {2:0.6f} asec;
enddef;"""

if options.vex:
    print ("$EOP;")
    eopformat = vexeop
else:
    eopformat = v2deop

# parse the eop page
neop = -1
nlines = None
for nlines, line in enumerate(eop_page):
    # skip the first line, which isn't commented
    if (nlines == 0):
        continue
    if not line:
        continue
    # strip comments
    if (line[0] == "#"):
        continue
    # split the line on whitespace and convert to floats
    eop_fields = [float(field) for field in line.split()]
    # print an EOP line if we're within 3 days of the target day
    if (abs(eop_fields[0] - target_jd) < 3):
        neop += 1
        tai_utc = get_leapsec(leapsec_page, eop_fields[0])
        if tai_utc is None:
            raise Exception("Leap seconds not found! Check your UT1LS file")
        xpole = eop_fields[1]/10.
        ypole = eop_fields[2]/10.
        ut1_utc = tai_utc+eop_fields[3]/1.e6
        eopdate = int(eop_fields[0] - mjd_jd)
        if options.vex:
            eopdate = espressolib.convertdate(eopdate, outformat="vex")
        print (eopformat.format(eopdate, xpole, ypole, tai_utc, ut1_utc, neop))

sys.stderr.write("Processed {0:d} lines\n".format(nlines))
