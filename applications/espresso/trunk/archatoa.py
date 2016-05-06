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

# archive FITS files to ATOA.
# Cormac Reynolds: Dec 2014

import optparse
import re
import subprocess


usage = """%prog <FITS_FILES>
will generate md5sums and transfer <FITS_FILES> to the ATOA staging area
"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "-l",
        type="str", dest="username", default=None,
        help="Specify your username on the ATOA host")

(options, args) = parser.parse_args()

if len(args) < 1:
    parser.print_help()
    parser.error("Give a list of FITS files")

fits_files = args


# directories on Horus for ATOA import
atoa_fits_dir = "horus.atnf.csiro.au:/data/ATOA_10/VLBI/"
atoa_md5sum_dir = "horus.atnf.csiro.au:/data/ATOA_10/md5sum/"

if options.username:
    atoa_fits_dir = options.username + "@" + atoa_fits_dir
    atoa_md5sum_dir = options.username + "@" + atoa_md5sum_dir


# format for ATOA is dd-mm-yy_hhmm.<projcode>.<whatever> where <projcode> is
# the official scheduling code for the project and <whatever> is arbitrary.
for fits_file in fits_files:
    header = open(fits_file).read(1000)
    date_obs = re.search("DATE-OBS=\s+'(\d{4}-\d{2}-\d{2})'", header).group(1)

    outfile = date_obs + "_0000." + fits_file
    md5sumfile = outfile + ".md5sum"

    # generate an md5sum file
    command = " ".join(["md5sum", fits_file, ">", outfile + ".md5sum"])
    print command
    subprocess.check_call(command, shell=True)

    # copy to FITS file to Horus, using the ATOA file name
    command = ["scp", fits_file, atoa_fits_dir + outfile]
    print command
    subprocess.Popen(command).communicate()

    # copy to md5sum file to Horus, using the ATOA file name
    command = ["scp", md5sumfile, atoa_md5sum_dir + outfile]
    print command
    subprocess.Popen(command).communicate()
