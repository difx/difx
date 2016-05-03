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

# simple script to take a v2d file and update the clock so that the reference
# date is the requested mjd (i.e. forward the rate from the given date to the
# requested date). The parser is a bit shoddy but will work for our usual files
# Cormac Reynolds: June 2010

import sys
import re
import optparse
import os
import mx.DateTime
import math


separator = "/"


def parseparam(param, line, delimiter="\S+"):
    line = line.strip(";")
    delimiter = r"\s*=\s*(" + delimiter + ")"
    value = re.search(param + delimiter, line).group(1)
    return value


def end_of_paragraph(line):
    # make sure not in quotes
    find_sep = False
    line = re.sub("'.*'", "", line)
    line = re.sub('".*"', '', line)
    if separator in line:
        find_sep = True

    return find_sep


# program starts here

# parse the options
usage = """%prog <site_id> <dbname> <expname.vex>
adjusts the position of <site_id> in <expname.vex> to be that of <dbname> in
$STADB"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")

(options, args) = parser.parse_args()
if len(args) != 3:
    parser.print_help()
    parser.error("3 arguments required")

vexfilename = args[2]
vexfile = open(vexfilename).readlines()
vexfile = [vexfile[i].strip("\n") for i in range(len(vexfile))]
vexfile = [vexfile[i].rstrip() for i in range(len(vexfile))]

station = args[0].upper()
pad_id = args[1].upper()


# read in the station db
stapath = os.getenv("STADB")
stadb = open(stapath).readlines()

padfound = False
cache = str()
for line in stadb:
    # remove comments
    line = re.sub("!.*", "", line)
    cache += line.upper()

    if end_of_paragraph(line):
        paragraph = cache
        cache = str()
        pad = parseparam("DBNAME", paragraph)

        if pad_id.upper() in pad:
            padfound = True
            pad_x = parseparam("X", paragraph)
            pad_y = parseparam("Y", paragraph)
            pad_z = parseparam("Z", paragraph)
            pad_dx = parseparam("DXDT", paragraph)
            pad_dy = parseparam("DYDT", paragraph)
            pad_dz = parseparam("DZDT", paragraph)
            pad_epoch = parseparam("EPOCH", paragraph)
            pad_frame = parseparam("FRAME", paragraph, "'.*'")

            break

if not padfound:
    raise Exception(pad_id + " not found in " + stapath)


# now process the vex file
siteblock = False
site_name = str()
site_id = str()
vexout = []
cache = []
comment = []
rate_adjust = float()
offset_adjust = float()
updatesite = False
sitefound = False
for line in vexfile:
    flushcache = True
    # extract the comments so we can put them back later
    try:
        comment.append(re.search(r"\*.*", line).group(0))
    except:
        comment.append("")

    # remove the comments so we can parse the line. Store the original line in
    # a cache so we can update it at the end of the v2d section if necessary
    line = re.sub("\*.*", "", line)
    cache.append(line)

    # find the site block
    if "$SITE" in line:
        siteblock = True
    # we leave the site block when we find the next block
    elif siteblock and "$" in line:
        siteblock = False

    # find the various names
    if siteblock:
        flushcache = False
        if re.match(r"\bdef\b", line):
            defname = re.search(r"def\s+(\w+)", line).group(1)
        if re.search(r"\bsite_name\b", line):
            site_name = parseparam(r"site_name", line).upper()
        if re.search(r"\bsite_ID\b", line):
            site_id = parseparam(r"site_ID", line).upper()

        # if the sitename or site_id matches the request, get ready to update
        # this def
        if site_name and site_id:
            if site_name == station or site_id == station:
                updatesite = True
                foundsite = True

        # at the end of the def, check if it's the station we're interested in.
        # Update the cached lines if we are.
        if "enddef" in line:
            flushcache = True
            site_name = str()
            site_id = str()
            defname = str()
            if updatesite:
                updatesite = False
                for i in range(len(cache)):
                    if "site_position" in cache[i]:
                        cache[i] = "*" + cache[i]
                    if "site_velocity" in cache[i]:
                        cache[i] = "*" + cache[i]
                    if "site_position_epoch" in cache[i]:
                        cache[i] = "*" + cache[i]

                indent = " " * 5
                sitepos = indent + "site_position = "
                for (pos, delim) in zip(
                        (pad_x, pad_y, pad_z), (":", ":", ";")):
                    sitepos += " " + pos + " m" + delim

                sitevel = indent + "site_velocity = "
                for (vel, delim) in zip(
                        (pad_dx, pad_dy, pad_dz), (":", ":", ";")):
                    sitevel += " " + vel + " m/yr" + delim

                epoch = indent + "site_position_epoch = "
                epoch += pad_epoch + ";"

                sitecomment = (
                        "* Added position of " + pad_id + " from " + stapath)
                cache.insert(-1, sitecomment)
                cache.insert(-1, "* " + pad_frame)
                cache.insert(-1, sitepos)
                cache.insert(-1, sitevel)
                cache.insert(-1, epoch)

    # write the new telescope information to the output array
    if flushcache:
        for i in range(len(cache)):
            try:
                cache[i] += comment[i]
            except:
                pass
        vexout += cache
        cache = []
        comment = []

if not foundsite:
    raise Exception(station + " not found in " + vexfilename)

# save a copy of the original before overwriting
os.rename(vexfilename, vexfilename + ".bak")
OUTVEXFILE = open(vexfilename, "w")

# print the output
for out_line in vexout:
    print>>OUTVEXFILE, out_line

OUTVEXFILE.close()
