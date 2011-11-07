from difxdb.model import model

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
           
    
    
