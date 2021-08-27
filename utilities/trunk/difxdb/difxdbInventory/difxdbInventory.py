#! /usr/bin/python3
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
from difxdb.business.experimentaction import *
from difxdb.business.filedataaction import *
from difxdb.model import model
from operator import  attrgetter
import argparse 

__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision: 7189 $"
__date__ ="$Date: 2016-01-15 13:47:46 +0100 (Fr, 15. Jan 2016) $"
__lastAuthor__="$Author: HelgeRottmann $"

def version():
    version = __prog__ + "\n"
    version += "Build: %s\n" %  __build__
    version += "Author: %s\n" %  __author__
    version += "Last Changed By: %s\n" %   __lastAuthor__

    return(version)

def usage():
    usage = ""
    usage += "%s   %s  %s (last changes by %s) \n" % (__prog__, __build__, __author__, __lastAuthor__)
    usage += "A program to list the current raw data inventory (modules and/or files).\n\n"
    usage += "NOTE: %s requires the DIFXROOT environment to be defined." % __prog__
    usage += "The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf."
    usage += "If the configuration is not found a sample one will be created for you."
    return(usage)
    

def listFiles(session):

        files = getFileData(session)
        sumScans = 0
        sumSizes = 0
        relCount = 0
        relSizes = 0
        for file in files:
            skip = False
            exp = getExperimentById(session, file.experimentID)
            typeStr = ""
            if file.numScans == 0 and file.size == 0:
                continue

            if args.type is not None:
                skip = True

                for type in exp.types:
                    typeStr += type.type + " "
                    if type.type.strip() in args.type:
                        skip = False
                        break
            if skip:
                continue

            if isExperimentReleased(session, exp.code):
                releaseStr = "*"
                relCount += 1
                relSizes += file.size
            else:
                releaseStr = " "

            sumScans +=  file.numScans
            sumSizes += file.size
            print (releaseStr, exp.code, file.stationCode, typeStr, file.numScans, file.size, file.location)

        print ("--------------------------------------------------")
        print ("Total data volume: %f TB in %d scans" % (sumSizes / 1e12, sumScans))
        if relCount > 0:
            print ("Total releasable data volume %f TB" % (relSizes / 1e12))
        print ("--------------------------------------------------")
        
def  listSlots(session):

        slots = getAllSlots(session)    

        print ("----------------------------------")
        print ("Slot  VSN")
        print ("----------------------------------")
        count = 0
        empties = 0
        for slot in slots:
                count += 1
                if not slot.module:
                        vsn = ""
                        empties += 1
                else:
                        vsn = slot.module.vsn

                print (slot.location, vsn)
        

        print ("--------------------------------------------------")
        print ("Total number of slots: %d" % count)
        print ("Filled/Empty: %d / %d" % (count-empties, empties))
        print ("--------------------------------------------------")
        return

if __name__ == "__main__":
    
    
    parser = argparse.ArgumentParser(usage=usage(), formatter_class=argparse.RawTextHelpFormatter)
    group = parser.add_mutually_exclusive_group()
    group.add_argument('-m','--modules-only', dest='modulesOnly', action='store_true', default=False, help='Consider only modules')
    group.add_argument('-f','--files-only', dest='filesOnly', action='store_true', default=False, help='Consider only files')
    parser.add_argument('-t','--type', action='append', help='List only media belonging to experiments with the given type(s)')
    parser.add_argument('-v','--version', action='version', version=version())
    args = parser.parse_args()

    try:
        if (os.getenv("DIFXROOT") == None):
            sys.exsit("Error: DIFXROOT environment must be defined.")

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

        if not args.filesOnly:
                listSlots(session)
        if not args.modulesOnly:
                listFiles(session)
        exit(0)

    

    except Exception as e:
       
        sys.exit(e)
    
   
    
