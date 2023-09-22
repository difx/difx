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
# $Id: dbutil.py 7191 2016-01-15 13:00:39Z HelgeRottmann $
# $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/libraries/python/difxutil/dbutil.py $
# $LastChangedRevision: 7191 $
# $Author: HelgeRottmann $
# $LastChangedDate: 2016-01-15 21:00:39 +0800 (五, 2016-01-15) $
#
#============================================================================

def none2String(variable):
    
    if variable is None:
        return("")
    else:
        return(variable)
