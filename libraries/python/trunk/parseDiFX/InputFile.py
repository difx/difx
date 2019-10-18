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

from .Common import get_common_settings, get_freqtable_info, get_telescopetable_info, get_datastreamtable_info, get_baselinetable_info

class InputFile:

    def __init__(self):
        self.common = None
        self.numfreqs, self.freqs = 0, None
        self.numtelescopes, self.telescopes = 0, None
        self.numdatastreams, self.datastreams = 0, None
        self.numbaselines, self.baselines = 0, None
        self.valid = False

    def fromfile(self, inputfile):
        self.common = get_common_settings(inputfile)
        self.numfreqs, self.freqs = get_freqtable_info(inputfile)
        self.numtelescopes, self.telescopes = get_telescopetable_info(inputfile)
        self.numdatastreams, self.datastreams = get_datastreamtable_info(inputfile)
        self.numbaselines, self.baselines = get_baselinetable_info(inputfile)
        self.valid = self.numfreqs >= 1 and self.numtelescopes >= 1 and \
            self.numdatastreams >= 1 and self.numbaselines >= 1

    def isvalid(self):
        return self.valid
