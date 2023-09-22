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
import collections
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.business.moduleaction import *
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.experimentaction import *
from difxdb.model import model
from difxfile.difxdir import DifxDir
from difxfile.difxfilelist import DifxFilelist
from operator import  attrgetter

__author__ = "Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__ = "$Revision$"
__date__ = "$Date$"
__lastAuthor__ = "$Author$"

def printUsage():
    print("%s   %s  %s (last changes by %s) \n" % (__prog__, __build__, __author__, __lastAuthor__))
    print("A program to construct the TAPELOG_OBS section from all modules in the database belonging to an experiment.\n")
    print("Usage: %s <experiment_code>\n\n"  % __prog__)
    print("NOTE: %s requires the DIFXROOT and MARK5_DIR_PATH environment variables to be defined." % __prog__)
    print("The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf.")
    print("If the configuration is not found a sample one will be created for you.")

    
    sys.exit(1)
    

if __name__ == "__main__":
    
    
    if (len(sys.argv) != 2):
        printUsage()
    
    expCode = sys.argv[1]
    try:
        if (os.getenv("DIFXROOT") == None):
            sys.exit("Error: DIFXROOT environment must be defined.")

        if (os.getenv("MARK5_DIR_PATH") == None):
            sys.exit("Error: MARK5_DIR_PATH environment must be defined.")

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

        if (experiment is None):
          sys.exit()

        sum = 0
        count = 0
        station = 0
        lastStationCode = ""
        entries = collections.defaultdict(list)

        # sort modules by station code
        sortedModules = sorted(experiment.modules, key= attrgetter('stationCode'))

        # look up start and stop times
        for i in range(len(sortedModules)):
            error = 0
            module = sortedModules[i]

            message = ""
            start = "UNKNOWN"
            stop =  "UNKNOWN"

            # read directories/filelists to find start and stop times
            dirFile = None
            try:
                if isMark6(module.vsn):
                    dirFile = DifxFilelist(os.getenv("MARK5_DIR_PATH"), module.vsn)
                else:
                    dirFile = DifxDir(os.getenv("MARK5_DIR_PATH"), module.vsn)
            except Exception as e:
                error +=1
                message = "# WARNING: %s" % e
                sys.stderr.write(message + "\n")

            if dirFile:
                    if dirFile.getExperimentStartDatetime(expCode.upper()):
                          try:
                                  start = dirFile.getExperimentStartDatetime(expCode.upper()).strftime("%Yy%jd%Hh%Mm%Ss")
                          except Exception as e:
                                  message = "WARNING: Error parsing {}".format(dirFile.getFilename())
                                  error += 1
                                  start = "UNKNOWN"
                    if dirFile.getExperimentStopDatetime(expCode.upper()):
                          try:
                                  stop = dirFile.getExperimentStopDatetime(expCode.upper()).strftime("%Yy%jd%Hh%Mm%Ss")
                          except Exception as e:
                                  message = "WARNING: Error parsing {}".format(dirFile.getFilename())
                                  error += 1
                                  stop =  "UNKNOWN"

            entries[module.stationCode].append ({'vsn': module.vsn, 'start': start, 'stop': stop, 'message': message})

        # now output the TAPELOG_OBS section
        print("$TAPELOG_OBS;")
        
        for key,value in entries.items():
            print("def " + key[0].upper() + key[1].lower() + ";")
            count = 0
            for module in sorted(value, key=lambda d: d['start']):

                print("VSN=%d :  %s :   %s : %s ;" % (count, module['vsn'], module["start"], module["stop"]))
                if len(module["message"])  > 0:
                    print (module["message"])
                count += 1

            print ("enddef;")

    except Exception as e:
        sys.exit(e)
    
   
    
