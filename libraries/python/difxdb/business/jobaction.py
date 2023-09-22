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
# $Id: jobaction.py 7191 2016-01-15 13:00:39Z HelgeRottmann $
# $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/libraries/python/difxdb/business/jobaction.py $
# $LastChangedRevision: 7191 $
# $Author: HelgeRottmann $
# $LastChangedDate: 2016-01-15 21:00:39 +0800 (五, 2016-01-15) $
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
