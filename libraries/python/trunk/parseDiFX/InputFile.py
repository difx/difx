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
        self.numoutputfreqs, self.outputfreqs = 0, None
        self.version = 2.6
        self.valid = False

    def fromfile(self, inputfile):
        self.common = get_common_settings(inputfile)
        self.numfreqs, self.freqs = get_freqtable_info(inputfile)
        self.numtelescopes, self.telescopes = get_telescopetable_info(inputfile)
        self.numdatastreams, self.datastreams = get_datastreamtable_info(inputfile)
        self.numbaselines, self.baselines = get_baselinetable_info(inputfile)
        self.numoutputfreqs, self.outputfreqs = self.determine_outputfreqs()
        self.version = self.determine_version() # TODO: currently relies on baseline table infos, make independent of that
        self.valid = self.numfreqs >= 1 and self.numtelescopes >= 1 and \
            self.numdatastreams >= 1 and self.numbaselines >= 0

    def isvalid(self):
        return self.valid

    def determine_version(self):
        version = 2.6
        if any([b.version >= 2.7 for b in self.baselines]):
            version = 2.7
        return version 

    def determine_outputfreqs(self):
        version = self.determine_version()
        if version <= 2.6:
            return self.__determine_outputfreqs_v26()
        else:
            return self.__determine_outputfreqs_v27()

    def __getFreqPolOfBand(self, ds, band):
        """Look up the frequency and polarization of a given band"""
        pol, fqId = '', -1
        if band < len(ds.recbandindex):
            recfreq = ds.recbandindex[band]
            fqId = ds.recfreqindex[recfreq]
            pol = ds.recbandpol[band]
        else:
            band = band - len(ds.recbandindex)
            recfreq = ds.zoombandindex[band]
            fqId = ds.zoomfreqindex[recfreq]
            pol = ds.zoombandpol[band]
        return (fqId,pol)

    def __determine_outputfreqs_v27(self):
        outputfqs = []
        for b in self.baselines:
            outputfqs += b.destfreq
        outputfqs = list(set(outputfqs))
        #print('v27', len(outputfqs), outputfqs)
        return len(outputfqs), outputfqs

    def __determine_outputfreqs_v26(self):
        outputfqs = []
        for b in self.baselines:
            ds1 = self.datastreams[b.dsaindex]
            ds2 = self.datastreams[b.dsbindex]
            if len(b.dsabandindex) != len(b.dsbbandindex):
                raise ValueError('Lenghts of ds<X>bandindex do not match (DS A: %d and DS B: %d)' % (len(b.dsabandindex),len(b.dsbbandindex)))
            for n in range(len(b.dsabandindex)):
                bandfq,bandpol = self.__getFreqPolOfBand(ds1, min(b.dsabandindex[n]))
                outputfqs.append(bandfq)
        outputfqs = list(set(outputfqs))
        #print('v26', len(outputfqs), outputfqs)
        return len(outputfqs), outputfqs
