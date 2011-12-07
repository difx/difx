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

def getModuleByVSN(session, vsn):
    
    return(session.query(model.Module).filter_by(vsn=vsn).one())
   

def isCheckOutAllowed(session, vsn):
    '''
    Checks whether a module can be checked out from the media library. If any
    of the experiments contained on the module have a status other than "released"
    check-out is not allowed.
    '''
    
    module = getModuleByVSN(session,vsn)
    
    if (module == None):
        return(False)
    
    for exp in module.experiments:
        if exp.status.statuscode < 100:
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