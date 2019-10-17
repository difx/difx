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

try:
    import numpy
    M_HAVE_NUMPY = True
except:
    M_HAVE_NUMPY = False

from .VisibilityHeader import VisibilityHeader

class VisibilityRecord:

    def __init__(self):
        self.header = VisibilityHeader()
        self.vis = None

    def fromfile(self, difxfile, freqtable):
        self.vis = None
        self.header.fromfile(difxfile)
        if not self.header.isvalid():
            return False
        fq = freqtable[self.header.freqindex]
        if fq.numchan % fq.specavg > 0:
            raise ValueError("DiFX input file error: channels %d not divisible by averaging factor %d" % (fq.numchan, fq.specavg))
        nchan = fq.numchan // fq.specavg
        if M_HAVE_NUMPY:
            vis = numpy.fromfile(difxfile, dtype='complex64', count=nchan)
            if len(vis) != nchan:
                return False
        else:
            vis = difxfile.read(8*nchan)
            if len(vis) != 8*nchan:
                return False
        self.vis = vis
        return True

    def isvalid(self):
        return self.header.isvalid()
