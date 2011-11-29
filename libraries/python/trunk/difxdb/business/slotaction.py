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