# To change this template, choose Tools | Templates
# and open the template in the editor.

from difxdb.model import model

def moduleExists(session, vsn):
    
    if (session.query(model.Module).filter_by(vsn=vsn).count() > 0):
        return(True)
    else:
        return(False)

def findModuleByVSN(session, vsn):
    
    return(session.query(model.Module).filter_by(vsn=vsn).one())
    
    
