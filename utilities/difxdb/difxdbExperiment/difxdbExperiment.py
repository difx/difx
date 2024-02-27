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
import argparse
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.experimentaction import *
from difxdb.model import model
from difxfile.difxdir import DifxDir
from operator import  attrgetter

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"

if __name__ == "__main__":
    
    epilog = "NOTE: %(prog)s requires the DIFXROOT environment variable to be defined."
    epilog += "The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf."
    epilog += "If the configuration is not found a sample one will be created for you."

    version =" %s " % ( __author__)

    parser =argparse.ArgumentParser(description="Obtain VLBI experiment information stored in difxdb", epilog = epilog)

    parser.add_argument('expcode', type=str, help="the code of the experiment")
    parser.add_argument('--version', action='version', version="%(prog)s" + version)
    parser.add_argument('--show-export', dest="showExport", action='store_true', default=False, help="show the exported files of this experiment.")
    parser.add_argument('--module-details', dest="moduleDetails", action='store_true', default=False, help="Show extended information for the modules used in this experiment.")

    args = parser.parse_args()

    expCode = args.expcode
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
        
        experiment = getExperimentByCode(session, expCode)
        if not experiment:
            sys.exit("Unknown experiment")

        types = ""
        for expType in experiment.types:
                types += expType.type + " "

        analyst = "none"

        if (experiment.user):
            analyst = experiment.user.name
        print("---------------------------------------")
        print("Summary information for %s" % (expCode))
        print("---------------------------------------")
        print("Number:\t\t%d" % (experiment.number))
        print("Type:\t\t%s" % (types))
        print("Analyst:\t%s" % (analyst))
        print("Status:\t\t%s" % (experiment.status.experimentstatus))
        print("Comment:\t\t%s" % (experiment.comment))
        print("---------------------------------------")

        sortedModules = sorted(experiment.modules, key= attrgetter('stationCode'))
        for module in sortedModules:
            if (args.moduleDetails):    
                print(module.stationCode,  module.vsn, module.slot.location, module.capacity, module.datarate, module.numScans)
            else:
                print(module.stationCode,  module.vsn, module.slot.location)

        if (args.showExport):
          if (len(experiment.exportFiles) > 0  ):
            print("---------------------------------------------------------------------------")
            print("Exported files (filename, path, checksum)")
            print("---------------------------------------------------------------------------")
          for export in experiment.exportFiles:
            print(("%s %s %s" %(export.filename, export.exportPath, export.checksum )))

    except Exception as e:
        import traceback
        traceback.print_exc()
       
        sys.exit(e)
    
   
    
