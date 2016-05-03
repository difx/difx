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

# Cormac Reynolds, December 2011: almost trivial install script for espresso
import os
import sys
import shutil
import optparse
import re

usage = """%prog <difxroot>
will install the scripts in <difxroot>/bin
"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
(options, args) = parser.parse_args()

difxroot = args[0]
if not difxroot:
    raise RuntimeError("DIFXROOT must be given")

difxbin = difxroot + os.sep + "bin"

if not os.path.exists(difxbin):
    raise RuntimeError(difxbin + " does not exist!")

dirlist = os.listdir(os.environ.get("PWD"))

for file in dirlist:
    if (
            re.search(".py$", file) or re.search(".pl$", file) or
            re.search(".sh$", file)):
        print file, difxbin
        shutil.copy2(file, difxbin)
