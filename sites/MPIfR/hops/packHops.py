#!/usr/bin/env python
# coding: latin-1
#===========================================================================
# Copyright (C) 2017  Max-Planck-Institut für Radioastronomie, Bonn, Germany
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

import sys
import os
import re
import tarfile
import argparse
from shutil import rmtree

reHops = re.compile("^\d{4}$")

tarDirs = []

description = "Program to search for fourfit subdirectories (e.g. 1234) recursively. All the discovered directories will be tar'ed, gzip'ed and the original directory will be removed."

parser = argparse.ArgumentParser(description=description)
parser.add_argument('-z', "--zip", action='store_true' , dest="compress", default=False, help='Compress tar-archive with gzip.')
parser.add_argument('-l', "--list", action='store_true' , dest="list", default=False, help='Only list discovered HOPS subdirectories. Do not pack.')
parser.add_argument('rootdir', help='The root directory underneath which to start finding the HOPS directories.')
args = parser.parse_args()

print "Searching for HOPS subdirectories...."
for subdir, dirs, files in os.walk(args.rootdir):
    for dir in dirs:
        # check for fourfit subdirectories (4-digit numeric)
        if reHops.match(dir):
          print "Found: " + subdir + "/" + dir
          tarDirs.append(subdir + "/" + dir)

if args.list:
	sys.exit(0)
	

for dir in tarDirs:

    # check that path exist (could have been packed already)
    if os.path.isdir(dir) == False:
	continue

    path,code = os.path.split(dir)

    if args.compress:
      filename = dir + ".tar.gz"
      mode = "w:gz"
    else:
      filename = dir + ".tar"
      mode = "w"

    print "creating: " + filename
    # open tar file (will overwrite if exist)
    try:
      tar = tarfile.open (filename, mode)
      tar.add(dir, arcname=code)
      tar.close()
    except:
      sys.exit("An error has occured when creating the tarball: " + filename)
  
    # remove hops subdirectory
    try:
        print "Removing: " + dir
        rmtree(dir)
    except OSError as ex:
        print(ex)

print "Done"
