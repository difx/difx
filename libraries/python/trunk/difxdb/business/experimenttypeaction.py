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

def getTypes(session):
    """
    Returns a list of all experiment types
    """
    return(session.query(model.ExperimentType).order_by("id").all())

def getActiveTypes(session):
    """
    Returns a list of all acive experiment types
    """
    return(session.query(model.ExperimentType).filter_by(active=1).order_by("id").all())
    
