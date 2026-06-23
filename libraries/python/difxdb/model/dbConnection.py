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
__author__="Helge Rottmann"

from sqlalchemy import *
from sqlalchemy.orm import *
from difxdb.model.model import *
#from model.model import *
from sqlalchemy.orm import registry


class Schema(object):
    """Describes the schema and mappers used by  SQLAlchemy """
    
    def __init__(self, connection):
        
        connStr = connection.getConnectionString()

        self.engine__ = create_engine(connStr, echo=connection.echo, pool_recycle=1000) 
        self.connection__ = self.engine__.connect()
    
        self.metadata__ = MetaData()
        self.registry__ = registry()
        
        self.metadata__.reflect(bind=self.engine__)
     
        self.loadSchema()
        self.createMappers()

    def _get_session(self):
        return scoped_session(sessionmaker(bind=self.engine__))
    session = property(_get_session)
    
    def loadSchema(self):
        
        self.experimentTable = Table("Experiment", self.metadata__, autoload_with=self.engine__)
        self.experimentStatusTable = Table("ExperimentStatus", self.metadata__, autoload_with=self.engine__)
        self.experimentTypeTable = Table("ExperimentType", self.metadata__, autoload_with=self.engine__)
        self.slotTable = Table("Slot", self.metadata__, autoload_with=self.engine__)
        self.moduleTable = Table("Module", self.metadata__, autoload_with=self.engine__)
        self.jobTable = Table("Job", self.metadata__, autoload_with=self.engine__)
        self.jobStatusTable = Table("JobStatus", self.metadata__, autoload_with=self.engine__)
        self.passTable = Table("Pass", self.metadata__, autoload_with=self.engine__)
        self.passTypeTable = Table("PassType", self.metadata__, autoload_with=self.engine__)
        self.versionHistoryTable = Table("VersionHistory", self.metadata__, autoload_with=self.engine__)
        self.userTable = Table("User", self.metadata__, autoload_with=self.engine__)
        self.exportFileTable = Table("ExportFile", self.metadata__, autoload_with=self.engine__)
        self.fileDataTable = Table("FileData", self.metadata__, autoload_with=self.engine__)
        self.experimentStatusHistoryTable = Table("ExperimentStatusHistory", self.metadata__, autoload_with=self.engine__)
        
        #association table for many-to-many Experiment/Module relation 
        self.experimentModuleTable = Table('ExperimentAndModule', self.metadata__, autoload_with=self.engine__)
        self.experimentAndTypeTable = Table('ExperimentAndType', self.metadata__, autoload_with=self.engine__)
        #self.experimentAndExportFileTable = Table('ExperimentAndExportFile', self.metadata__, autoload_with=self.engine__)
  
        

        
    def createSchema(self):

        #self.slotTable = Table('Slot', self.metadata__,
        #    Column('id', Integer, primary_key=True),
        #    Column('location', String(20)))

        #self.metadata__.create_all(self.engine__)
        pass

    def createMappers(self):

        clear_mappers()

        self.registry__.map_imperatively(Queue, self.jobTable, properties={'Pass':relationship(Pass, uselist=False),'status':relationship(JobStatus, uselist=False)})
        self.registry__.map_imperatively(Job, self.jobTable, properties={'status':relationship(JobStatus, uselist=False)})
        self.registry__.map_imperatively(JobStatus, self.jobStatusTable)
        self.registry__.map_imperatively(Pass, self.passTable, properties={'experiment':relationship(Experiment, uselist=False), 'type':relationship(PassType, uselist=False)})
        self.registry__.map_imperatively(PassType, self.passTypeTable)
        self.registry__.map_imperatively(ExperimentStatus, self.experimentStatusTable)
        self.registry__.map_imperatively(Experiment, self.experimentTable, properties={'status':relationship(ExperimentStatus, uselist=False, lazy="subquery"), \
            'user':relationship(User, primaryjoin=self.experimentTable.c.userID==self.userTable.c.id, uselist = False, lazy="subquery"), \
            'releasedByUser':relationship(User, primaryjoin=self.experimentTable.c.releasedByUserID==self.userTable.c.id, uselist = False, lazy="subquery"), \
            'types':relationship(ExperimentType, secondary=self.experimentAndTypeTable, primaryjoin=self.experimentAndTypeTable.c.experimentID==self.experimentTable.c.id, secondaryjoin=self.experimentAndTypeTable.c.experimentTypeID==self.experimentTypeTable.c.id, foreign_keys = [self.experimentAndTypeTable.c.experimentID, self.experimentAndTypeTable.c.experimentTypeID], lazy="subquery")}) 

        self.registry__.map_imperatively(Module, self.moduleTable, properties={'experiments': relationship(Experiment, secondary=self.experimentModuleTable, primaryjoin=self.experimentModuleTable.c.moduleID==self.moduleTable.c.id, secondaryjoin=self.experimentModuleTable.c.experimentID==self.experimentTable.c.id, foreign_keys = [self.experimentModuleTable.c.experimentID, self.experimentModuleTable.c.moduleID], backref=backref('modules'))}) 
        self.registry__.map_imperatively(Slot, self.slotTable,properties={'module': relationship(Module, uselist = False, backref=backref('slot', uselist=False))})
        self.registry__.map_imperatively(VersionHistory, self.versionHistoryTable)
        self.registry__.map_imperatively(User, self.userTable)
        self.registry__.map_imperatively(ExperimentType, self.experimentTypeTable)
        self.registry__.map_imperatively(ExperimentStatusHistory, self.experimentStatusHistoryTable)
        self.registry__.map_imperatively(FileData, self.fileDataTable, properties={'experiment': relationship(Experiment, backref=backref('fileData'))})
        self.registry__.map_imperatively(ExportFile, self.exportFileTable, properties={'experiment': relationship(Experiment, backref=backref('exportFiles'))})


class Connection(object):
    
    def __init__(self, difxdbConfig = None):
        
        self.type = ""
        self.server = ""
        self.port = ""
        self.user = ""
        self.password = ""
        self.database = ""
        
        # if a configuration object was passed
        # use it to fill the connection parameters
        if difxdbConfig is not None:
            self.type = difxdbConfig.get("Database", "type")
            self.server = difxdbConfig.get("Database", "server")
            self.port = difxdbConfig.get("Database", "port")
            self.user = difxdbConfig.get("Database", "user")
            self.password = difxdbConfig.get("Database", "password")
            self.database = difxdbConfig.get("Database", "database")
        
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
