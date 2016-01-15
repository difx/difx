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
__author__="Helge Rottmann"

import ConfigParser
import os

class DifxDbConfig(object):
    
    def __init__(self, configFile, create=False):
        '''
        Constructor.
        Checks if the configuration file exists. If yes it will be read.
        If the file does not exist but create is set to 'True' a default 
        configuration file will be created. If create is set to 'False' (default)
        an IO Exception will be raised
        '''
        
        self.configFile = configFile
        self.config = ConfigParser.RawConfigParser()
        
        if (not os.path.isfile(configFile)):
            if (create):
                self.makeDefaultConfig()
                self.writeConfig()
            else:
                raise IOError("Configfile: %s does not exist" % configFile)
        
        
        self.read()
        
        
    def read(self):
        '''
        Reads the configuration file
        '''
        
        self.config.read(self.configFile)
        
    def makeDefaultConfig(self):
        '''
        Creates a minimal default (database) configuration 
        '''
        
        self.config.add_section('Database')
        self.config.set('Database', 'server', 'localhost')
        self.config.set('Database', 'port', '3306')
        self.config.set('Database', 'user', 'db_user')
        self.config.set('Database', 'password', 'db_password')
        self.config.set('Database', 'database', 'difxdb')
        self.config.set('Database', 'type', 'mysql')
        
    def writeConfig(self):
        '''
        Saves the configuration to disk
        '''
        
        # Writing  configuration file 
        with open(self.configFile, 'w') as configfile:
            self.config.write(configfile)
    
    def get(self, section, key):
        '''
        Returns the configuration value for key in section 
        If the section or key does not exist an empty string is returned
        '''
        try:
            value = self.config.get(section,key)
        except:
            value = ""
            
        return(value)
    
    def set(self, section, key, value):
        '''
        Sets the configuration value for key in section 
        ''' 
        return(self.config.set(section, key, value))
    
    
    def sectionExists(self, section):
        '''
        Checks whether a section already exists in the configuration. Returns true in case the section exists, false otherwise.
        '''
        return(self.config.has_section(section))
    
        
    def addSection(self, section):
        '''
        Adds a new section to the configuration if it does not exist yet. Note: in order to make this change
        permanent writeConfig needs to be called.
        '''
        
        if (not self.config.has_section(section)):
            self.config.add_section(section)
    
