__author__="Helge Rottmann"
__date__ ="$Sep 12, 2011 9:35:56 AM$"

from sqlalchemy import *
from sqlalchemy.orm import *
from difxdb.model.model import *

class Schema(object):
    """Describes the schema and mappers used by  SQLAlchemy """
    
    def __init__(self, connection):
        
        connStr = connection.getConnectionString()
        self.engine__ = create_engine(connStr, echo=connection.echo) 
        self.connection__ = self.engine__.connect()
    
        self.metadata__ = MetaData(self.engine__)
     
        
        self.loadSchema()
        self.createMappers()
    

    def _get_session(self):
        return scoped_session(sessionmaker(bind=self.engine__))
    session = property(_get_session)

    def loadSchema(self):
             
        self.experimentTable = Table("Experiment", self.metadata__, autoload=True)
        self.experimentStatusTable = Table("ExperimentStatus", self.metadata__, autoload=True)
        self.slotTable = Table("Slot", self.metadata__, autoload=True)
        self.moduleTable = Table("Module", self.metadata__, autoload=True)
        
        #association table for many-to-many Experiment/Module relation 
        self.experimentModuleTable = Table('ExperimentAndModule', self.metadata__, autoload=True)
  
        

        
    def createSchema(self):

        self.slotTable = Table('Slot', self.metadata__,
            Column('id', Integer, primary_key=True),
            Column('location', String(20)))

        self.metadata__.create_all(self.engine__)

    def createMappers(self):
        
        clear_mappers()
        mapper(ExperimentStatus, self.experimentStatusTable)
        mapper(Experiment, self.experimentTable,properties={'status':relation(ExperimentStatus, uselist = False)})
        mapper(Module, self.moduleTable, properties={'experiments': relation(Experiment, secondary=self.experimentModuleTable, primaryjoin=self.experimentModuleTable.c.moduleID==self.moduleTable.c.id, secondaryjoin=self.experimentModuleTable.c.experimentID==self.experimentTable.c.id, foreign_keys = [self.experimentModuleTable.c.experimentID, self.experimentModuleTable.c.moduleID])})
        mapper(Slot, self.slotTable,properties={'module': relation(Module, uselist = False, backref='slot')})
        

class Connection(object):
    
    def __init__(self):
        
        self.type = ""
        self.server = ""
        self.port = ""
        self.user = ""
        self.password = ""
        self.database = ""
        
        self.echo = False
        
    def getConnectionString(self):
        
        try:
            self._validate()
        except:
            raise
            return("")
        
        str = self.type + "://" + self.user + ":" + self.password + "@" + self.server + ":" + self.port + "/" + self.database
        
        return(str)
    
    def _validate(self):
        
        if self.type == "":
            raise  ConnectionParamError("Database type has not been specified")
        if self.server == "":
            raise  ConnectionParamError("Database server has not been specified")
        if self.port == "":
            raise  ConnectionParamError("Database port has not been specified")
        if self.user == "":
            raise  ConnectionParamError("Database user has not been specified")
        if self.password == "":
            raise  ConnectionParamError("Database password has not been specified")
        if self.database == "":
            raise  ConnectionParamError("Database has not been specified")
       

        
class ConnectionParamError(Exception):
    
    def __init__(self, value):
        self.value = value
        
    def __str__(self):
        return repr(self.value)
