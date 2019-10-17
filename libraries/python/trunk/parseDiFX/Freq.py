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

class Freq:

    def __init__(self):
        self.bandwidth = 0.0
        self.freq = 0.0
        self.lsb = False
        self.numchan = 0
        self.specavg = 0
        self.oversamplefac = 1
        self.decimfac = 1
        self.npcal = 0
        self.pcalindices = []

    def str(self):
        sideband = "LSB" if self.lsb else "USB"
        summary = "%12.6f MHz %3s [%4d-ch/%d-avg] @ %.6f MHz" % (self.bandwidth,sideband,self.numchan,self.specavg,self.freq)
        return summary

    def __eq__(self, other): 
        if isinstance(other, Freq):
            eq = (self.freq == other.freq) and (self.bandwidth == other.bandwidth) and (self.lsb == other.lsb)
            eq = eq and (self.numchan // self.specavg == other.numchan // other.specavg)
        else:
            eq = (self.freq == other)
        return eq
