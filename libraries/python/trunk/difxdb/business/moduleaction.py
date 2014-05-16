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
import os

def moduleExists(session, vsn):
    
    if (session.query(model.Module).filter_by(vsn=vsn).count() > 0):
        return(True)
    else:
        return(False)

def getModuleById(session, id):
    
    return(session.query(model.Module).filter_by(id=id).one())
   
   
def getModuleByVSN(session, vsn):
    
    return(session.query(model.Module).filter_by(vsn=vsn).one())
   

def isCheckOutAllowed(session, vsn):
    '''
    Checks whether a module can be checked out from the media library. If any
    of the experiments contained on the module have a status other than "released"
    check-out is not allowed. Also the directory for the module must have been
    parsed at least once in order to allow check-out. However check-out is allowed
    for unscanned modules if an experiment was assigned manually and the experiment
    is released.
    '''
    expCount = 0 
    module = getModuleByVSN(session,vsn)
    
    if (module == None):
        return(False)
    
    # all experiments contained on this module must be released
    for exp in module.experiments:
	expCount += 1
        if exp.status.statuscode < 100:
            return(False)
        
    # Modules that do not have an experiment associated with them must have been
    # scanned at least once in order to allow check-out.
    if module.numScans is None and expCount == 0:
        return(False)

    return(True)
           
def hasDir(vsn, dirPath=None):
    '''
    Checks whether a .dir file exists for the module with the given vsn.
    If the optional dirPath is set the .dir file will be searched under that path.
    Otherwise the MARK5_DIR_PATH environment is evaluated.
    '''
    if (dirPath == None):
        dirPath = os.getenv("MARK5_DIR_PATH")
        if (dirPath == None):
            return(False)
             
        if (os.path.isfile(dirPath + "/" + vsn + ".dir")):
            return(True)
    
        
    return(False)
    
    
def getUnscannedModules(session):
    '''
    Returns a collection of Module objects that have not been scanned 
    '''
    return(session.query(model.Module).filter(model.Module.numScans == None).all())


def getScanCount(session, vsn):
    '''
    Returns the number of scans recorded on the module. None if the module has not been scanned yet.
    '''
    
    try:
        module = getModuleByVSN(session, vsn)
    except:
        return
    
    return(module.numScans)
    
