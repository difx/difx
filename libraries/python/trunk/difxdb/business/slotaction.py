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

def slotExists(session, location):
    
    if (session.query(model.Slot).filter_by(location=location).count() > 0):
        return(True)
    else:
        return(False)
    
def getSlotByLocation(session, location):
    
    return(session.query(model.Slot).filter_by(location=location).one())
    
def getOccupiedSlots(session):
    '''
    Returns a collection of Slot objects that are currently occupied by modules 
    '''
    
    return(session.query(model.Slot).filter(model.Slot.moduleID != None).order_by(model.Slot.location).all())

def getEmptySlots(session):   
    '''
    Returns a collection of Slot objects that are currently not occupied by modules 
    '''
    
    result =  session.query(model.Slot).order_by(model.Slot.location).filter_by(isActive = 1).order_by(model.Slot.location).filter(model.Slot.moduleID == None)
    
    return(result)