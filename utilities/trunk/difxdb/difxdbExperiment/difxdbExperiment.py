#! /usr/bin/python
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
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

import os
import sys
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.experimentaction import *
from difxdb.model import model
from difxfile.difxdir import DifxDir
from operator import  attrgetter

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision$"
__date__ ="$Date$"
__lastAuthor__="$Author$"

def printUsage():
    print "%s   %s  %s (last changes by %s) \n" % (__prog__, __build__, __author__, __lastAuthor__)
    print "A program to print summary information for an experiment.\n"
    print "Usage: %s <experiment_code>\n\n"  % __prog__
    print "NOTE: %s requires the DIFXROOT environment variable to be defined." % __prog__
    print "The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf."
    print "If the configuration is not found a sample one will be created for you."

    
    sys.exit(1)
    

if __name__ == "__main__":
    
    
    if (len(sys.argv) != 2):
        printUsage()
    
    expCode = upper(sys.argv[1])
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
        
	try:
		experiment = getExperimentByCode(session, expCode)
	except:
		raise Exception("Unknown experiment")
		sys.exit

	types = ""
	for expType in experiment.types:
		types += expType.type

	print "---------------------------------------"
	print "Summary information for %s" % (expCode)
	print "---------------------------------------"
	print "Number:\t\t%d" % (experiment.number)
	print "Type:\t\t%s" % (types)
	print "Analyst:\t%s" % (experiment.user.name)
	print "Status:\t\t%s" % (experiment.status.experimentstatus)
	print "Comment:\t\t%s" % (experiment.comment)
	print "---------------------------------------"
	print "Modules (see file TAPELOG_OBS for details)"

	sortedModules = sorted(experiment.modules, key= attrgetter('stationCode'))
	for module in sortedModules:
		print module.stationCode,  module.vsn, module.slot.location
    except Exception as e:
       
        sys.exit(e)
    
   
    
