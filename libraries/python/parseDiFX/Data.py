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
# $LastChangedDate: 2019-10-17 14:09:33 +0200 (Thu, 17 Oct 2019) $
#
#============================================================================

class Data:
    '''
    Storage class for holding information from
    the DATA TABLE in DiFX input file 
    '''

    def __init__(self):
        self.datastreamindex = -1
        self.media = []

    def __str__(self):
        return str(self.__class__) + ": " + str(self.__dict__)

