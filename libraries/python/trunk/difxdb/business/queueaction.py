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
