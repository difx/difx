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
from sqlalchemy import and_


def passExists(session, passName, expCode):
    '''
    Checks if a pass with the given name exists for the given experiment
    Returns True if the pass exists, False otherwise
    '''
    
    if (session.query(model.Pass).join(model.Experiment).filter(and_(model.Pass.passName==passName, model.Experiment.code==expCode)).count() > 0):
        return(True)
    else:
        return(False)

def addPass(session, passName, expId):
    '''
    Adds an pass to the database.
    '''
    
    if (passExists(session, passName, expId)):
        return

    newPass = model.Pass()
    newPass.passName = passName
    newPass.experimentID = expId
    
    session.add(newPass)
    session.commit()
    
def getPass(session, passName, expCode):
    
    return(session.query(model.Pass).join(model.Experiment).filter(and_(model.Pass.passName==passName, model.Experiment.code==expCode)).one())
 
def getPasses(session, expCode, passNames=[]):
    result = []
    
    for instance in  session.query(model.Pass).join(model.Experiment).filter(model.Experiment.code==expCode):
        if len(passNames) > 0:
            if (instance.Pass.passName not in passName):
                continue
        result.append(instance)
        
    return(result)
 
def passTypeExists(session, passType):
    
    if (session.query(model.PassType).filter_by(type=passType).count() > 0):
        return(True)
    else:
        return(False)
    
def getPassType(session, typeName):
    
    return(session.query(model.PassType).filter_by(type=typeName).one())