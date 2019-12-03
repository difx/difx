# -*- coding: utf-8 -*-
#===========================================================================
# Copyright (C) 2019  Max-Planck-Institut für Radioastronomie, Bonn, Germany
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
# $HeadURL: $
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

from __future__ import division

from .Common import parse_output_header, M_SYNC_WORD

class VisibilityHeader:

    def __init__(self):
        self.clear()

    def clear(self):
        self.raw = ''
        self.syncword = 0
        self.baseline = 0
        self.mjd = 0
        self.seconds = 0
        self.configindex = 0
        self.srcindex = 0
        self.freqindex = 0
        self.polpair = 0
        self.pulsarbin = 0
        self.weight = 0
        self.u, self.v, self.w = 0, 0, 0
        self.uvw = [self.u, self.v, self.w]
        # Derived
        self.antenna1 = 0
        self.antenna2 = 0

    def fromfile(self, difxfile):
        h = parse_output_header(difxfile)
        if len(h) < 12:
            self.clear()
            return False
        self.raw = h[-1]
        self.syncword = M_SYNC_WORD
        self.baseline, self.mjd, self.seconds = h[0:3]
        self.configindex, self.srcindex, self.freqindex = h[3:6]
        self.polpair = h[6]
        self.pulsarbin = h[7]
        self.weight = h[8]
        self.u, self.v, self.w = h[9:12]
        self.uvw = [self.u, self.v, self.w]
        self.antenna2 = self.baseline % 256
        self.antenna1 = (self.baseline - self.antenna2) // 256
        return True

    def isvalid(self):
        return self.syncword == M_SYNC_WORD
