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
from difxdb.business.experimentaction import *
from difxdb.business.passaction import *
 
def getJobs(session, expCode, passName, numbers=[]):
    
    result = []
    for instance in  session.query(model.Queue).join(model.Pass).join(model.Experiment).filter(and_(model.Experiment.code==expCode,model.Pass.passName==passName)):
        if (len(numbers) > 0):
            if instance.jobNumber not in numbers:
                continue
        result.append(instance) 
        
    return(result)

def getActiveJobs(session, expCodes=[]):
    '''
    Returns the list of jobs that have an active job status. If expCodes is given
    incomplete jobs are returned only if they correspond to one of the experiments
    found in the list
    '''
    
    result = []
    
    for instance in  session.query(model.Queue).join(model.JobStatus).filter(model.JobStatus.active==1):
        if (len(expCodes) > 0):
            if (instance.Pass.experiment.code  not in expCodes):
                continue
        result.append(instance)
        
    return(result)

def getActiveJobsByPasses(session, expCode, passes):
    
    result = []
    
    
    for instance in  session.query(model.Queue).join(model.JobStatus).join(model.Pass).join(model.Experiment).filter(and_(model.Experiment.code==expCode, model.JobStatus.active==1)):
    
        if (len(passes) > 0):
            if (instance.Pass.passName  not in passes):
                continue
        result.append(instance)
        
    return(result)

def addQueueItem(session, job, expCode, passName):
    
    queueItem = model.Queue()
    
    # add experiment if it doesn't exist yet
    if not experimentExists(session, expCode):
        addExperiment(session, expCode)
    
    queueExp = getExperimentByCode(session, expCode)
    
    # add pass if it doesn't exist yet
    if not passExists(session, passName, expCode):
        addPass(session, passName, queueExp.id)
    
    queuePass = getPass(session, passName, expCode)
    queuePass.experiment = queueExp
    
    # now construct Job object
    queueItem = job
    queueItem.statusID = 2
    queueItem.Pass = queuePass
    queueItem.passID = queuePass.id
        
    session.add(queueItem)
    session.commit()
    
    
def deleteQueueItem(session, queueItem):
    
    session.delete(queueItem)
    session.commit()
