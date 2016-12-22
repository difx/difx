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
# $Id: getmodules.py 7189 2016-01-15 12:47:46Z HelgeRottmann $
# $HeadURL: https://svn.atnf.csiro.au/difx/utilities/trunk/difxdb/getmodules/getmodules.py $
# $LastChangedRevision: 7189 $
# $Author: HelgeRottmann $
# $LastChangedDate: 2016-01-15 13:47:46 +0100 (Fr, 15. Jan 2016) $
#
#============================================================================

import os
import sys
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.slotaction import *
from difxdb.model import model
from operator import  attrgetter
from optparse import OptionParser

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision: 7189 $"
__date__ ="$Date: 2016-01-15 13:47:46 +0100 (Fr, 15. Jan 2016) $"
__lastAuthor__="$Author: HelgeRottmann $"

def usage():
    usage = ""
    usage += "%s   %s  %s (last changes by %s) \n" % (__prog__, __build__, __author__, __lastAuthor__)
    usage += "A program to list all modules associated with an experiment\n"
    usage += "Usage: %s <experiment_code>\n\n"  % __prog__
    usage += "NOTE: %s requires the DIFXROOT environment to be defined." % __prog__
    usage += "The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf."
    usage += "If the configuration is not found a sample one will be created for you."
    return(usage)

    
    sys.exit(1)
    

def  listSlots(session):

	slots = getAllSlots(session)	

	for slot in slots:
		if not slot.module:
			vsn = ""
		else:
			vsn = slot.module.vsn

		print slot.location, vsn
	

	return

if __name__ == "__main__":
    
    
    parser = OptionParser(usage=usage())


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

	listSlots(session)
	exit(0)

    

	if (experiment is not None):
                sortedModules = sorted(experiment.modules, key= attrgetter('stationCode'))
		for module in sortedModules:
			print module.vsn, module.slot.location, module.stationCode, module.capacity
			count += 1
			sum += module.capacity

		print "--------------------------------------------"
		print "Total capacity: ", sum, "GB", " on", count , "modules"
        
    
    except Exception as e:
       
        sys.exit(e)
    
   
    
