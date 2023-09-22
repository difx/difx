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
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model.dbConnection import Schema, Connection
from difxdb.business.experimentaction import *
from difxdb.business.moduleaction import *
from difxdb.business.slotaction import *

from difxdb.model import model
from operator import  attrgetter
from optparse import OptionParser


__author__="Helge Rottmann <rottmann@mpifr-bonn.mpg.de>"
__prog__ = os.path.basename(__file__)
__build__= "$Revision$"
__date__ ="$Date$"
__lastAuthor__="$Author$"

def getUsage():

        usage = "%prog [options] [<experiment1> [<experiment2>] ...]\n\n"
        usage += '\nA program to list all releasable modules.'
        usage += '\nOptionally one or more experiment codes can be given in order'
        usage += '\nto limit output to modules associated with these experiment(s).'
        usage += '\nFor possibilties to further filter the results please consult the options below.\n\n'
        usage += 'NOTE: The program requires the DIFXROOT environment to be defined.\n'
        usage += "The program reads the database configuration from difxdb.ini located under $DIFXROOT/conf.\n"
        usage += "If the configuration is not found a sample one will be created for you at this location.\n"
        return usage

def printLine(module):

    info = ""
    if module.numScans == None:
        info = "This module has never been scanned. Confirm contents before releasing." 
    print("%4s %8s %4s %5s %5s %s" % (module.slot.location, module.vsn, module.stationCode, module.datarate, module.capacity, info))
    
    

if __name__ == "__main__":
    
    usage = getUsage()
    version = "%s\nSVN  %s\nOriginal author: %s\nLast changes by: %s\nLast changes on: %s" % (__prog__, __build__, __author__, __lastAuthor__, __date__)
    #usage = "usage: %prog [options] arg1 arg2"

    parser = OptionParser(version=version, usage=usage)
    parser.add_option("-s", "--slot", dest="slot", default="", 
                  help="show only modules that are located in slots matching the given expression")
    parser.add_option("-e", "--extended", action="store_true", help="print extended information")
   
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
        
        expCodes = set()
        orphanModules = set()
        
      
        for code in args:
            try:
                experiment = getExperimentByCode(session, code)
            except:
                print("Unknown experiment %s" % code)
                continue
                    
            expCodes.add(experiment.code)

        
        totalCapacity = 0
        moduleCount = 0
        
        slots = getOccupiedSlots(session)
        
        modules = []
        
        for slot in slots:
            
            # apply slot filter
            if (options.slot != ""):
                if (not slot.location.startswith(options.slot)):
                    continue
          
            # check if module is releasable
            if (not isCheckOutAllowed(session, slot.module.vsn)):
                continue
            
            # apply experiment filter
            if len(args) != 0:
                match = False
                for exp in slot.module.experiments:
                    for code in args:
                        if exp.code == code:
                            match = True
                            break
                if (match == False):
                    continue
            else:
                # remember the experiment codes of all modules matching the filters 
                for exp in slot.module.experiments:
                    expCodes.add(exp.code)
    
            # remember modules without experiments
            if len(slot.module.experiments) == 0:
                orphanModules.add(slot.module)
                
            modules.append(slot.module)
            
            if (not options.extended):
                printLine(slot.module)
                
            totalCapacity += slot.module.capacity
            moduleCount += 1 
        
        # sort experiments alphabetically
        expCodes = sorted(expCodes)

        if (options.extended):
            for code in expCodes:

                print("\n------")
                print(code)
                print("------")
                for module in modules:
                    moduleExpCodes = []
                    for experiment in module.experiments:
                        moduleExpCodes.append(experiment.code)
                    #print moduleExpCodes
                    if code in moduleExpCodes:
                        printLine(module)
            
            if (len(orphanModules) > 0):
                print("\n------")
                print("No exp.")
                print("------")
                
                for module in orphanModules:
                    printLine(module)
            

        
        print("\n-------")
        print("Summary")
        print("-------")
        print("Number of modules: %d" % (moduleCount))
        print("Total capacity: %d" % (totalCapacity))
        
        sys.exit(0)
    
    except Exception as e:
       
        sys.exit(e)
    

    
