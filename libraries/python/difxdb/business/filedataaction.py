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
from difxdb.model import model

def getFileData(session):
    '''
    Returns the list of all data files.
    Returns None if no files have been found
    '''
    try:
        ret = session.query(model.FileData).all()
    except:
        ret = None
    return(ret)

def getFilesByExperimentId(session, expId):
    '''
    Returns a list of data files associated with the experiment of the given id.
    Returns None when no associated files have been found
    '''

    try:
        ret = session.query(model.FileData).filter_by(experimentID=expId).all()
    except:
        ret = None
    return(ret)


