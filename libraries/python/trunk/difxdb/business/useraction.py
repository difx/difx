# -*- coding: utf-8 -*-
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id:$
# $HeadURL:$
# $LastChangedRevision:$
# $Author:$
# $LastChangedDate:$
#
#============================================================================
from difxdb.model import model

def getUserByName(session, userName):
    '''
    Returns the User object that has the given name
    '''
    if userName == "":
        return None
    
    return(session.query(model.User).filter_by(name=userName).one())

def getEnabledUsers(session):   
    '''
    Returns a collection of all enabled User objects
    '''
    
    return(session.query(model.User).filter_by(enabled=1).order_by("id").all())
