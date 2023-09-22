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
# $Id: Datastream.py 9222 2019-10-17 12:09:33Z JanWagner $
# $HeadURL: $
# $LastChangedRevision: 9222 $
# $Author: JanWagner $
# $LastChangedDate: 2019-10-17 20:09:33 +0800 (四, 2019-10-17) $
#
#============================================================================

class Datastream:

    def __init__(self):
        self.telescopeindex = 0
        self.tsys = 0
        self.format = ""
        self.quantbits = 0
        self.framesize = 0
        self.datasampling = ""
        self.datasource = ""
        self.filterbankused = False
        self.phasecalint = 0
        self.nrecfreq = 0
        self.recfreqpols = []
        self.recfreqindex = []
        self.recclockoffset = []
        self.recfreqoffset = []
        self.recgainoffset = []
        self.nrecband = 0
        self.recbandpol = []
        self.recbandindex = []
        self.nzoomfreq = 0
        self.zoomfreqpols = []
        self.zoomfreqindex = []
        self.nzoomband = 0
        self.zoombandpol = []
        self.zoombandindex = []
        self.version = 2.6
