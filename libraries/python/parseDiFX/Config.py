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
# $Id: Config.py 9222 2019-10-17 12:09:33Z JanWagner $
# $HeadURL: $
# $LastChangedRevision: 9222 $
# $Author: JanWagner $
# $LastChangedDate: 2019-10-17 20:09:33 +0800 (四, 2019-10-17) $
#
#============================================================================

class Config:

    def __init__(self):
        self.name = ""
        self.inttime = 0.0
        self.subintns = 0
        self.guardns = 0
        self.fringerotorder = 0
        self.arraystridelen = 0
        self.xmacstridelen = 0
        self.numbufferedffts = 0
