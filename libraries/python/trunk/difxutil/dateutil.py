# -*- coding: utf-8 -*-
#===========================================================================
# Copyright (C) 2016  Max-Planck-Institut für Radioastronomie, Bonn, Germany
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
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================
from datetime import *

def mjd2vexTime (mjd, dateonly=False):
	'''
	Converts a Mean Julian Date (MJD) into a vex compatible datetime
	If the parameter dateonly is set to True only the date portion will be returned
	If dateonly is set to False (default) the full datetime string will be returned
	'''

	
	if mjd < 50001 or mjd > 99999:
		return ''

	mjd0 = datetime(1858, 11, 17, 0, 0)
	md = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]

	d = int(mjd)
	s = int((mjd - d)*86400.0 + 0.5)
	dt = timedelta(d, s)
	t = mjd0 + dt
	d = t.day + md[t.month-1]
	if t.year % 4 == 0 and t.month > 2:
		d += 1
	if dateonly:
		return '%dy%03dd' % \
			(t.year, d)
	else:
		return '%dy%03dd%02dh%02dm%02ds' % \
			(t.year, d, t.hour, t.minute, t.second)

