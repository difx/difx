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

import glob

from .InputFile import InputFile
from .VisibilityRecord import VisibilityRecord

class DiFXFile:

    def __init__(self):
        self.metainfo = InputFile()
        self.visrecord = VisibilityRecord()
        self.inputfilename = ''
        self.difxfilename = ''
        self.difxfile = None
        self.valid = False

    def open(self, inputfilename):
        self.metainfo.fromfile(inputfilename)
        self.valid = self.metainfo.isvalid()
        if not self.valid:
            return
        
        glob_pattern = self.metainfo.common['difxfile'] + '/DIFX_*.s*.b*'
        difxfileslist = glob.glob(glob_pattern)

        if len(difxfileslist) <= 0:
            print ('Error: no visibility data file found in %s!' % (glob_pattern))
            raise ValueError('No visibility files %s as referenced by %s were found' % (glob_pattern,inputfilename))

        self.difxfilename = difxfileslist[0]
        self.difxfile = open(self.difxfilename, 'rb')
        self.valid = True

    def close(self):
        self.difxfile.close()

    def isvalid(self):
        return self.valid

    def currentVisibilityRecord(self):
        return self.visrecord

    def nextVisibilityRecord(self):
        self.visrecord.fromfile(self.difxfile, self.metainfo.freqs)
        return self.visrecord

    def getFrequency(self, freqindex):
        return self.metainfo.freqs[freqindex]

    def getTelescope(self, telescopeindex):
        return self.metainfo.telescopes[telescopeindex]
