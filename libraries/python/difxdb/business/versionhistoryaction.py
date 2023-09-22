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
from sqlalchemy import desc

def getCurrentSchemaVersionNumber(session):
    '''
    Returns the major and minor version numbers of the current schema. The current schema is determined by
    the dateCreated field in the versionhistory table.
    '''
    
    version = session.query(model.VersionHistory).order_by(desc(model.VersionHistory.dateCreated)).first()
    return( version.major, version.minor)

def isSchemaVersion(session, major, minor):
    '''
    Compares the major and minor version numbers of the current schema against the major and minor versions
    passed to  the routine as arguments. Returns True if the schema major and minor version are equal or greater
    than the arguments, False otherwise.
    '''
    
    dbMajor, dbMinor = getCurrentSchemaVersionNumber(session)
    dbVersion = dbMajor*1000 + dbMinor
    version = major*1000 + minor
    
    if (version <= dbVersion):
        return(True)
    else:
        return(False)
