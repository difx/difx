#!/usr/bin/env python2
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

# copy DiFX log to separate job log files.
# Cormac Reynolds: April 2018

import optparse
import os
import re
import sys

usage = """%prog <difxlog> <outdir>
will separate log messages for jobs in <difxlog> into separate job logs in
<outdir>
"""
parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "-e", "--expname",
        dest="expname", type=str, default=None,
        help="Specify experiment name (else inferred from input filename)")

(options, args) = parser.parse_args()

if len(args) != 2:
    parser.print_help()
    parser.error("Give input .difxlog and output directory")

explog_filename = args[0]
outdir = args[1]

expname = None
if options.expname:
    expname = options.expname
else:
    expname = "".join(explog_filename.split(".")[0:-1])

joblist = []
outfile = {}
with open(explog_filename, "r") as infile:
    for lineno, line in enumerate(infile):
        jobname = re.search(expname+"\S*", line).group(0)
        outfilename = os.path.join(outdir, jobname + ".difxlog")
        if jobname not in joblist:
            # Open file if not opened before. Rename any pre-existing version.
            print "Found {:s} at line {:d}".format(jobname, lineno)
            joblist.append(jobname)
            if os.path.exists(outfilename):
                bakfile = outfilename + ".bak"
                sys.stderr.write(
                        "Renaming {:s} to {:s}\n".format(outfilename, bakfile))
                os.rename(outfilename, bakfile)
            outfile[outfilename] = open(outfilename, "a")
        outfile[outfilename].write(line)

# close all the output files
for filename in outfile.keys():
    outfile[filename].close()
