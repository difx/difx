# -*- coding: utf-8 -*-
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
