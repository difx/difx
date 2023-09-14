#!/usr/bin/env python3
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
# Cormac Reynolds, September 2023: use the standard DiFX install invocation


from __future__ import print_function, division
import os
import sys
import shutil
import optparse
#import re

verb = False
force = True
errors = []


def run(cmd):
    # Subroutine to run a command and raise error on non-zero return - from DiFX install.py
    if verb:
        print("Run(" + cmd + ") in " + os.getcwd())
    sys.stdout.flush()
    if os.system(cmd):
        if force:
            errors.append(os.getcwd() + ' ' + cmd + " failed.")
        else:
            raise RuntimeError("Error running " + cmd + " in " + os.getcwd())


usage = """%prog <difxroot>
will install the scripts in <difxroot>/bin and espressolib <difxroot>/lib/python
"""

parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
(options, args) = parser.parse_args()

try:
    difxroot = args[0]
except:
    difxroot = os.environ.get("DIFXROOT", None)
if difxroot is None:
    raise RuntimeError("$DIFXROOT must be set or <difxroot> must be given as argument")

run("aclocal")
run("autoconf")
run("automake -a")
run("make install")
