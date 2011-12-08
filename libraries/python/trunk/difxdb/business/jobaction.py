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
#from sqlalchemy import desc


def getIncompleteJobs(session):
    '''
    Returns the list of jobs that have a state 
    '''
    
    result = []
    
    for instance in  session.query(model.Job).join(model.JobStatus).filter(model.JobStatus.ative==1):
        result.append(instance.code)
        
    return(result)

def jobStatusExists(session, jobStatus):
    '''
    Checks if the given job status exists. 
    Returns True if the job status exists, False otherwise
    '''
    
    if (session.query(model.JobStatus).filter_by(status=jobStatus).count() > 0):
        return(True)
    else:
        return(False)
    
def getJobStatus(session, jobStatus):
    
    return(session.query(model.JobStatus).filter_by(status=jobStatus).one())