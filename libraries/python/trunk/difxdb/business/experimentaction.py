from difxdb.model import model

def experimentExists(session, code):
    '''
    Checks if an experiment with the given code exists. 
    Returns True if the experiment exists, False otherwise
    '''
    
    if (session.query(model.Experiment).filter_by(code=code).count() > 0):
        return(True)
    else:
        return(False)
    
def getActiveExperimentCodes(session):
    '''
    Determines all active experiments. Experiments are considered to be active if their state is not "released". 
    Returns a list containing the experiment codes
    '''
    result = []
    
    for instance in  session.query(model.Experiment).join(model.ExperimentStatus).filter(model.ExperimentStatus.statuscode<100).order_by(model.Experiment.code):
        result.append(instance.code)
        
    return(result)
    