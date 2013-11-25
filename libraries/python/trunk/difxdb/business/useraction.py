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

def getEnabledUsers(session):   
    '''
    Returns a collection of all enabled User objects
    '''
    
    result =  session.query(model.User).filter_by(enabled = 1);
    
    return(result)


