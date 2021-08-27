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
import string
import argparse
import logging
from logging.handlers import TimedRotatingFileHandler
from  datetime import datetime
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.filedataaction import *
from difxdb.business.experimentaction import *
from difxdb.model import model

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision$"
__date__ ="$Date$"
__lastAuthor__="$Author$"

logger = None

description = "A script to parse the filesystem for file-based correlator data and populate the difxdb database."
epilog = "The script assumes that under the given root-path subdirectories named after the experiment code exist.\n"
epilog += "Below the experiment directory subdirectories with the two-letter station codes are expected.\n\n"
epilog += "NOTE: %(prog)s requires the DIFXROOT environment to be defined.\n" 
epilog += "The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf.\n"
epilog += "If the configuration is not found a template will be created for you.\n\n"
epilog += "The output is logged to file (see also --log-path option)"
version = "revision = {} author = {}  last changed by = {}".format(__build__, __author__, __lastAuthor__)

    
def setupLoggers(logPath):


    if args.verbose:
        level = logging.DEBUG
    else:
        level = logging.INFO

    logger = logging.getLogger("difxdbParseFiles")
    logger.setLevel(level)

    # logging to file
    #fh = logging.FileHandler(logPath + "/difxdbParseFiles_{:%Y-%m-%d_%H:%M:%S}.log".format(datetime.now()))
    fh = TimedRotatingFileHandler(logPath+"/difxdbParseFiles.log", when="midnight", interval=1, backupCount=7)

    fh.setLevel(level)
    fh.setFormatter(logging.Formatter('%(asctime)s - %(levelname)-7s - %(message)s'))

    # console handler
    ch = logging.StreamHandler()
    ch.setLevel(level)
    ch.setFormatter(logging.Formatter('%(levelname)-7s - %(message)s'))

    logger.addHandler(fh)
    logger.addHandler(ch)
 
    return logger

def storeItem(session, storedFiles, exp, item):

        if not item:
                return

        logger.debug("Validating {}".format(item["location"]))
                
        # check if this is a new entry
        update = False
        for f in storedFiles:
                if f.location == item["location"]:
                        # compare sizes and number of files
                        if f.size == item["totalSize"] and f.numScans == item["fileCount"]:
                                logger.debug("Not updating: Identical entry in database for path={}".format(f.location))
                                return

                        # record needs to be updated
                        update = True
                        break
                        
        if update:
                fileData = f
                fileData.size = item["totalSize"]
                fileData.numScans = item["fileCount"]
                fileData.received = datetime.now()
        else: # new record
                fileData = model.FileData()
                fileData.stationCode = item["stationCode"]
                fileData.location = item["location"]
                fileData.experimentID = item["expId"]
                fileData.size = item["totalSize"]
                fileData.numScans = item["fileCount"]
                session.add(fileData)

        if not args.dryRun:
                session.commit()
                session.flush()
                
        if update:
                logger.info("Updated database entry for path={}".format(f.location))
        else:
                logger.info("Added database entry for path={}".format(item["location"]))
                
        
        
def parseFS(session, rootPath):

        logger.info("Parsing directory {}".format(rootPath))
        # assume to level directory is equal to experiment code
        for top in os.listdir(rootPath):
                expDir = rootPath + "/" + top
                if not os.path.isdir(expDir):
                        continue
                item = {}
                item["code"] = top.upper()
                code = top.upper()

                # check if a corresponing experimt exists in the database
                exp = getExperimentByCode(session, code)
                if not exp:
                        logger.info ("Skipping subdirectory: {}. No experiment with code {} found in database.".format(expDir, code))
                        continue

                item["expId"] = exp.id

                # get list of files already stored for this experiment
                storedFiles = getFilesByExperimentId(session, exp.id)

                # assume second level directories represent the 2-letter station code
                for second in os.listdir(expDir):
                        stationDir = expDir + "/" + second

                        if not os.path.isdir(stationDir):
                                continue

                        if len(second) != 2:
                                stationCode = ""
                        else:
                                stationCode = second.upper()

                        item["stationCode"] = stationCode

                        # count the files
                        fileCount = 0
                        totalSize = 0
                        for file in os.listdir(stationDir):
                                filePath = stationDir + "/" + file
                        
                                if not os.path.isfile(filePath):
                                        logger.warning("The station directory {} contains a subdirectory {}. Skipping.".format(stationDir, file))
                                        continue
                                fileCount += 1
                                totalSize += os.path.getsize(filePath)
                                
                        item["location"] = stationDir
                        item["fileCount"] = fileCount
                        item["totalSize"] = totalSize

                        storeItem(session, storedFiles, exp, item)

                        
        session.close
        return

if __name__ == "__main__":
    
    
    parser = argparse.ArgumentParser(description=description, epilog=epilog ,formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("rootPath", help='the path of the root directory to search for station data.')
    parser.add_argument("-d", "--dry-run", dest="dryRun", action="store_true", default=False, help="report action only; do not update the database.")
    parser.add_argument("--verbose", action="store_true", default=False, help="verbose output.")
    parser.add_argument("--log-path", dest="logPath",  default="/tmp", help="path where the log files will be written to (default: /tmp).")
    parser.add_argument('--version', action='version', version=version)

    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(1)
        
    args = parser.parse_args()

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

        logger = setupLoggers(args.logPath)
        parseFS(session, args.rootPath)
        exit(0)

    except Exception as e:
       
        sys.exit(e)
    
   
    
