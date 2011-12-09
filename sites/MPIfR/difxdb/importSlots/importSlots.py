#! /usr/bin/python
#---------------------------------------------------------
# Program to import the slots.txt file used by tuba /ttuby
# into the difxdb database
#---------------------------------------------------------
import sys
import os
import os.path
from datetime import datetime
from difxdb.difxdbconfig import DifxDbConfig
from difxdb.model import model
from difxdb.business.slotaction import slotExists, getSlotByLocation
from difxdb.model.dbConnection import Schema, Connection

__author__="Helge Rottmann"
__date__ ="$08.12.2011 10:09:35$"

if __name__ == "__main__":
    
    if len(sys.argv) < 2:
        sys.stderr.write('Usage: importSlots {slots_filename}')
        sys.exit(1)

    if (os.getenv("DIFXROOT") == None):
        sys.exit("Error: environment variable DIFXROOT must be defined.")
        
    configFile = os.getenv("DIFXROOT") + "/conf/difxdb.ini"
    if not os.path.isfile(configFile):
        sys.exit("Error: Configuration file does not exist: %s" % configFile)
    
    config = DifxDbConfig(configFile, create=False)
    
    file = open(sys.argv[1], "r")
    
    connection = Connection(config)
    connection.echo = False
    
    dbConn = Schema(connection)
    session = dbConn.session()
    
    for line in file:

	inDateStr = ""
	inDatetime = None

        fields = line.split()
        if (len(fields) > 3):
      
            oldSlot = fields[0]
            vsn = fields[3]
            
            if (vsn == "--------"):
                continue

	    if (len(fields) > 5):
		inDateStr = fields[5]
		try:
			inDatetime = datetime.strptime(inDateStr, '%y%j')
		except ValueError as e:
			print "Illegal checkin date (%s) found for module %s" % (inDateStr, vsn)

         
            if (slotExists(session, oldSlot)):
                module = model.Module()
                module.vsn = vsn
                module.capacity = 0
                module.datarate = 0
		if (inDatetime != None):
			module.received = inDatetime
                
                slot = getSlotByLocation(session, oldSlot)
                
                if (slot != None):
                    slot.vsn = vsn
                    slot.module = module
                    
                    try:
                        session.commit()
                    except Exception  as e:
                        print e
                        exit() 
                    print "Added %s to slot %s" % ( vsn, oldSlot)
            else:
                print "WARNING: Slot %s not found in database" % slot
