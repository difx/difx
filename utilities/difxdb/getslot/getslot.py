#! /usr/bin/env python3
# coding: latin-1

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

import os
import sys
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.moduleaction import getModuleByVSN, moduleExists
from difxdb.model import model

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)

def printUsage():
    print("%s   %s  \n" % (__prog__,  __author__))
    print("A program to get the location of a disk module\n")
    print("Usage: %s <VSN>\n\n"  % __prog__)
    print("%s requires the DIFXROOT environment to be defined." % __prog__)
    print("The program will read the database configuration from difxdb.ini located under $DIFXROOT/conf.")
    print("If the configuration is not found a sample one will be created for you.")

    
    sys.exit(1)

if __name__ == "__main__":
    
    
    if (len(sys.argv) != 2):
        printUsage()
    
    vsn = sys.argv[1]
    try:
        if (os.getenv("DIFXROOT") == None):
            sys.exit("Error: DIFXROOT environment must be defined.")

        configPath = os.getenv("DIFXROOT") + "/conf/difxdb.ini"


        config = DifxDbConfig(configPath, create=True)

        # try to open the database connection
        connection = Connection()
        connection.type = config.get("Database", "type")
        connection.server = config.get("Database", "server")
        connection.port = config.get("Database", "port")
        connection.user = config.get("Database", "user")
        connection.password = config.get("Database", "password")
        connection.database = config.get("Database", "database")
        connection.echo = False

        dbConn = Schema(connection)
        session = dbConn.session()
        
        if (moduleExists(session,vsn)):
            module = getModuleByVSN(session, vsn)
        
            print(module.slot.location)
        else:
            print("Unknown")
        
        
    
    except Exception as e:
       
        sys.exit(e)
    
   
    
