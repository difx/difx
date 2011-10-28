from difxdb.model import model

def getOccupiedSlots(session):
    '''
    Returns a collection of Slot objects that are currently occupied by modules 
    '''
    
    return(session.query(model.Slot).filter(model.Slot.moduleID != None).all())