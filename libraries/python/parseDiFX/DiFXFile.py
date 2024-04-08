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

import glob

from .InputFile import InputFile
from .VisibilityRecord import VisibilityRecord

class DiFXFile:

    def __init__(self, inputfilename=None):
        self.metainfo = InputFile()
        self.visrecord = VisibilityRecord()
        self.inputfilename = ''
        self.difxfilename = ''
        self.difxfilepath = ''
        self.difxfile = None
        self.valid = False
        if inputfilename:
            self.open(inputfilename)

    def open(self, inputfilename, phasecenterId=None, pulsarbinId=None):
        self.metainfo.fromfile(inputfilename)
        self.valid = self.metainfo.isvalid()
        if not self.valid:
            return
        
        self.difxfilepath = self.metainfo.common['difxfile']

        glob_pattern = self.difxfilepath + '/DIFX_*'
        if phasecenterId is not None:
            glob_pattern = glob_pattern + '.s%04d' % (phasecenterId)
        else:
            glob_pattern = glob_pattern + '.s*'
        if pulsarbinId is not None:
            glob_pattern = glob_pattern + '.b%04d' % (pulsarbinId)
        else:
            glob_pattern = glob_pattern + '.b*'

        difxfileslist = glob.glob(glob_pattern)
        if len(difxfileslist) <= 0:
            print(('Warning: no visibility data file found in %s!' % (glob_pattern)))
        else:
            self.difxfilename = difxfileslist[0]
            self.difxfile = open(self.difxfilename, 'rb')

    def close(self):
        self.difxfile.close()

    def isvalid(self):
        return self.valid

    def currentVisibilityRecord(self):
        return self.visrecord

    def nextVisibilityRecord(self):
        if self.difxfile is not None:
            self.visrecord.fromfile(self.difxfile, self.metainfo.freqs)
        return self.visrecord

    def getFrequency(self, freqindex):
        return self.metainfo.freqs[freqindex]

    def getTelescope(self, telescopeindex):
        return self.metainfo.telescopes[telescopeindex]
