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
    Returns the Experiment object referenced by the given experiment code
    '''
    
    return(session.query(model.Experiment).filter_by(code=code).one())

def getExperimentStatusCode(session, code):
    '''
    Returns the status code of the experiment with the given experiment code
    '''
    exp = session.query(model.Experiment).filter_by(code=code).one()
    return(exp.status.statuscode)
    
def getActiveExperimentCodes(session):
    '''
    Determines all active experiments. Experiments are considered to be active if their state is not "released". 
    Returns a list containing the experiment codes
    '''
    result = []
    
    for instance in  session.query(model.Experiment).join(model.ExperimentStatus).filter(model.ExperimentStatus.statuscode<100).order_by(model.Experiment.code):
        result.append(instance.code)
        
    return(result)

def _addExperiment(session,experiment):
    
    if (experimentExists(session, experiment.code)):
        return
 
def addExperiment(session, code, types=[], analyst=None, statuscode=0):
    '''
    Adds an experiment to the database. New experiments receive the default
    state (=unknown). 
    '''
    
    if (experimentExists(session, code)):
        return

    expTypes = []
    experiment = model.Experiment()
    experiment.code = upper(code)
    experiment.number = int(getLastExperimentNumber(session)) + 1
    experiment.user = analyst

    try:
	for type in types:
		expType= session.query(model.ExperimentType).filter_by(type=type).one()
                if expType is not None:
                    expTypes.append(expType)
    except:
	raise Exception("Trying to set an unknown epxeriment type (%s)" (type))

    experiment.types = expTypes
    

    try:
        status = session.query(model.ExperimentStatus).filter_by(statuscode=statuscode).one()
        experiment.status = status
    except:
        raise Exception("Trying to set an unknown statuscode (%s)" % (statuscode))

    try:
        session.add(experiment)
        session.commit()     
    except:
        raise Exception("Error adding experiment")
        session.rollback()
        
    session.flush()
        
def addExperimentWithState(session, code, statuscode):
    '''
    Adds an experiment to the database giving it the statuscode
    '''
    
    if (experimentExists(session, code)):
        return

    experiment = model.Experiment()
    experiment.code = upper(code)
    experiment.number = int(getLastExperimentNumber(session)) + 1
    
    try:
        status = session.query(model.ExperimentStatus).filter_by(statuscode=statuscode).one()
    except:
        raise Exception("Trying to set an unknown statuscode (%s)" % (statuscode))
    
    experiment.status = status
    
    try:
        session.add(experiment)
        session.commit()       
    except:
        raise Exception("Error adding experiment")
        session.rollback()
        
    session.flush()
    
def deleteExperimentByCode(session, code):
    '''
    Deletes the experiment with the given code
    '''
    
    
    experiment = session.query(model.Experiment).filter_by(code=code).one()
     
    if (experiment is None):
        return
    # check if there are modules linked to this experiment
    if (len(experiment.modules) > 0):
        raise Exception("Experiment %s cannot be deleted because it has associated modules" % experiment.code)
    
    # delete associatons to ExperimentType
    #for type in experiment.types:
#	experiment.remove(type)

    session.delete(experiment)
    try:
        session.commit() 
    except:
        raise Exception("Error deleting experiment")
        session.rollback()
        
    session.flush()
    
def isExperimentReleased(session, code):
    '''
    Checks if the given experiment has a status that indicates that it is released
    '''
    
    experiment = session.query(model.Experiment).filter_by(code=code).one()
    
    if (experiment is None):
        return
    
    # status codes > 100 means experiment is released
    if (experiment.status.statuscode >= 100):
        return(True)
    else:
        return(False)
    
def changeExperimentState(session, code, statuscode):
    '''
    Set the status of the given experiment to the status with the given statuscode
    Raises an Exception if the statuscode does not exist in the database
    '''
    try:
        experiment = session.query(model.Experiment).filter_by(code=code).one
    except:
        raise Exception ("Unknown experiment %s" % code)
    
    try:
        status = session.query(model.ExperimentStatus).filter_by(statuscode=statuscode).one()
    except:
        raise Exception("Trying to set an unknown statuscode (%s) for experiment %s" % (statuscode, code))
   
    experiment.status = status
    try:
        session.commit()
    except:
        session.rollback()

    session.flush()
    
def isExperimentArchived(session, code):
    '''
    Checks whether the given experiment has been archived. 
    Returns True / False.
    Raises an exception if the experiment does not exist in the database
    '''
    try:
        experiment = session.query(model.Experiment).filter_by(code=code).one()
        
        if (experiment.dateArchived is None):
            return(False)
        else:
            return (True)
        
    except:
        raise Exception ("Unknown experiment %s" % code)
    
