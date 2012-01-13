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
from string import upper


def experimentExists(session, code):
    '''
    Checks if an experiment with the given code exists. 
    Returns True if the experiment exists, False otherwise
    '''
    
    if (session.query(model.Experiment).filter_by(code=code).count() > 0):
        return(True)
    else:
        return(False)

def getLastExperimentNumber(session):
    '''
    Retrieves the highest experiment number assigned so far.
    '''
    exp = session.query(model.Experiment.number).order_by(desc(model.Experiment.number)).first()
    return(exp.number)

def getExperiments(session):
    '''
    Returns a list of all Experiment objects found in the database
    '''
    return(session.query(model.Experiment).all())

def getExperimentByCode(session, code):
    '''
    Returns the Experiment object referencesd by the given experiment code
    '''
    
    return(session.query(model.Experiment).filter_by(code=code).one())
    
def getActiveExperimentCodes(session):
    '''
    Determines all active experiments. Experiments are considered to be active if their state is not "released". 
    Returns a list containing the experiment codes
    '''
    result = []
    
    for instance in  session.query(model.Experiment).join(model.ExperimentStatus).filter(model.ExperimentStatus.statuscode<100).order_by(model.Experiment.code):
        result.append(instance.code)
        
    return(result)
 
def addExperiment(session, code):
    '''
    Adds an experiment to the database.
    '''
    
    if (experimentExists(session, code)):
        return

    experiment = model.Experiment()
    experiment.code = upper(code)
    experiment.number = int(getLastExperimentNumber(session)) + 1
    

    session.add(experiment)
    session.commit()
    
def deleteExperimentByCode(session, code):
    
    
    experiment = session.query(model.Experiment).filter_by(code=code).one()
     
    if (experiment is None):
        return
    # check if there are modules linked to this experiment
    if (len(experiment.modules) > 0):
        raise Exception("Experiment %s cannot be deleted because it has associated modules" % experiment.code)
    
    session.delete(experiment)
    session.commit()