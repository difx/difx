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
import math

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


def mjdToDate(mjd):
    '''
    Convert a Mean Julian Date (MJD) into year, month and day
    representation
    '''
    jd = float(mjd) + 0.5 + 2400000.5
    
    F, I = math.modf(jd)
    I = int(I)
    
    A = math.trunc((I - 1867216.25)/36524.25)
    
    if I > 2299160:
        B = I + 1 + A - math.trunc(A / 4.)
    else:
        B = I
        
    C = B + 1524
    
    D = math.trunc((C - 122.1) / 365.25)
    
    E = math.trunc(365.25 * D)
    
    G = math.trunc((C - E) / 30.6001)
    
    day = C - E + F - math.trunc(30.6001 * G)
    
    if G < 13.5:
        month = G - 1
    else:
        month = G - 13
        
    if month > 2.5:
        year = D - 4716
    else:
        year = D - 4715
        
    return year, month, day