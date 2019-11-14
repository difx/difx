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
from difxdb.business.moduleaction import getModuleByVSN, moduleExists
from difxdb.model import model
from optparse import OptionParser

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision$"
__date__ ="$Date$"
__lastAuthor__="$Author$"

def printUsage():
    usage = ""
    usage +=  "%s [options] VSN\n\n"  % __prog__
    usage +=  "A program to get information about the disk module with the given VSN.\n"
    usage +=  "%s requires the DIFXROOT environment to be defined." % __prog__
    usage +=  "The program will read the database configuration from difxdb.ini located under $DIFXROOT/conf."
    usage +=  "If the configuration is not found a sample one will be created for you."

    return usage

def printSummary():
    print "Slot:    %s" % module.slot.location
    print "Station: %s" % module.stationCode
    print "Scans:   %s" % module.numScans


if __name__ == "__main__":
    
    usage = printUsage()
    version = "%s\nSVN  %s\nOriginal author: %s\nLast changes by: %s\nLast changes on: %s" % (__prog__, __build__, __author__, __lastAuthor__, __date__)
    parser = OptionParser(version=version, usage=usage)
    parser.add_option("-s", "--slot", action="store_true", dest="slot", default=False, help="show the library slot")
    parser.add_option("-S", "--station", action="store_true", dest="station", default=False, help="show the 2-letter station code")
    parser.add_option("-n", "--num-scans", action="store_true", dest="numScans", default=False, help="show the number of scans")

    (options, args) = parser.parse_args()
   
    if (len(args) != 1):
	parser.print_help()
	sys.exit(1)
    
    vsn = args[0]

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

	# check if module exists in the DB
        if (moduleExists(session,vsn)):
            module = getModuleByVSN(session, vsn)
        else:
            print "Unknown"
	    sys.exit(0)
	    
	# check for options
	summary = True
	if (options.slot == True):
            print module.slot.location
            summary = False
	if (options.station == True):
            print module.stationCode
            summary = False
	if (options.numScans == True):
            print module.numScans
            summary = False

	if summary == True:
	    printSummary()
	
        
    
    except Exception as e:
       
        sys.exit(e)
    
   
    
