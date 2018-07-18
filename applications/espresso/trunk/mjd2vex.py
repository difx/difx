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

# convert between DiFX date formats.
# Cormac Reynolds: June 2010

import optparse
import espressolib

# parse the options
usage = """%prog [options] <date>
converts <date> between DiFX date formats (mjd, vex, iso, vlba).
Multiple dates can be given (separated by spaces)
"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "2.0")
parser.add_option(
        "--format", "-f",
        type="str", dest="outformat", default=None,
        help="Output format (vex, vlba, iso, mjd). "
        "Default is mjd unless first input format is mjd, then vex is default")
(options, args) = parser.parse_args()

if len(args) < 1:
    parser.print_help()
    parser.error("At least 1 date required")

# by default, convert to MJD except for MJD which converts to vex
outformat = options.outformat
if outformat is None:
    try:
        indate = float(args[0])
        outformat = "vex"
    except ValueError:
        outformat = "mjd"
outformat = outformat.lower()

for indate in args:
    print espressolib.convertdate(indate, outformat)
