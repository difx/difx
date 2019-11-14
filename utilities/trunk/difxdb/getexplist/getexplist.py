#! /usr/bin/env python2
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
# $Id: getreleaselist.py 4932 2012-10-09 12:47:22Z HelgeRottmann $
# $HeadURL: https://svn.atnf.csiro.au/difx/utilities/trunk/difxdb/getreleaselist/getreleaselist.py $
# $LastChangedRevision: 4932 $
# $Author: HelgeRottmann $
# $LastChangedDate: 2012-10-09 14:47:22 +0200 (Tue, 09 Oct 2012) $
#
#============================================================================

import os
import sys
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.experimentaction import *
from difxdb.business.moduleaction import *
from difxdb.business.slotaction import *

from difxdb.model import model
from operator import  attrgetter
from optparse import OptionParser
from datetime import datetime

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision: 4932 $"
__date__ ="$Date: 2012-10-09 14:47:22 +0200 (Tue, 09 Oct 2012) $"
__lastAuthor__="$Author: HelgeRottmann $"

def getUsage():

	usage = "%prog [options] ]\n\n"
	usage += '\nA program to list all experiments stored in the database.'
        usage += 'NOTE: The program requires the DIFXROOT environment to be defined.\n'
        usage += "The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf.\n"
        usage += "If the configuration is not found a sample one will be created for you at this location.\n"
        return usage

def printLine(exp):

    dateCreated = ""
    if exp.dateCreated is not None:
	dateCreated = exp.dateCreated.date()
    print "%4s %9s %12s %20s" % (exp.number, exp.code, dateCreated,
exp.status.experimentstatus)
    
    

if __name__ == "__main__":
    
    usage = getUsage()
    version = "%s\nSVN  %s\nOriginal author: %s\nLast changes by: %s\nLast changes on: %s" % (__prog__, __build__, __author__, __lastAuthor__, __date__)
    #usage = "usage: %prog [options] arg1 arg2"

    parser = OptionParser(version=version, usage=usage)
    #parser.add_option("-s", "--slot", dest="slot", default="", 
    #              help="show only modules that are located in slots matching the given expression")
    #parser.add_option("-e", "--extended", action="store_true", help="print extended information")
   
    (options, args) = parser.parse_args()
    
    
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
        
        expCount = 0;
        experiments = getExperiments(session)
	sortedExps = sorted(experiments, key=attrgetter('number'))
	
	
	print "----------------------------------------------------------------"
    	print "%4s %9s %12s %20s" % ("Num.", "Code", "Date created", "Status")
	print "----------------------------------------------------------------"
        for exp in sortedExps:
		expCount +=1
		printLine(exp)

	print "----------------------------------------------------------------"
	print "Total: " ,expCount , " experiments"
	print "----------------------------------------------------------------"
        
        sys.exit(0)
    
    except Exception as e:
       
        sys.exit(e)
    

    
