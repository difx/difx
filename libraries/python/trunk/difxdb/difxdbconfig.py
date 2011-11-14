__author__="Helge Rottmann"
__date__ ="$11.11.2011 10:35:30$"


import ConfigParser
import os

class DifxDbConfig(object):
    
    def __init__(self, configFile, create=False):
        '''
        Constructor.
        Checks if the configuration file exists. If yes it will be read.
        If the file does not exist but create is set to 'True' a default 
        configuration file will be created. If create is set to 'False' (default)
        an IO Exption will be raised
        '''
        
        self.configFile = configFile
        self.config = ConfigParser.RawConfigParser()
        
        if (not os.path.isfile(configFile)):
            if (create):
                self.createConfig()
            else:
                raise IOError("Configfile: %s does not exist" % configFile)
        
        
        self.read()
        
        
    def read(self):
        '''
        Reads the configuration file
        '''
        
        self.config.read(self.configFile)
        
    
    def createConfig(self):
        '''
        Creates a minimal default (mysql) configuration 
        '''
        
        self.config.add_section('Database')
        self.config.set('Database', 'server', 'localhost')
        self.config.set('Database', 'port', '3306')
        self.config.set('Database', 'user', 'db_user')
        self.config.set('Database', 'password', 'db_password')
        self.config.set('Database', 'database', 'difxdb')
        self.config.set('Database', 'type', 'mysql')
        
        # Writing  configuration file 
        with open(self.configFile, 'w') as configfile:
            self.config.write(configfile)
    
    def get(self, section, key):
        '''
        Returns the configuration value for key in section 
        ''' 
        return(self.config.get(section,key))
    
    
    
