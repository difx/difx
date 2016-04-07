#!/usr/bin/env python
#=======================================================================
# Copyright (C) 2016 Cormac Reynolds
#                                                                       
# This program is free software; you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by  
# the Free Software Foundation; either version 3 of the License, or     
# (at your option) any later version.                                   
#                                                                       
# This program is distributed in the hope that it will be useful,       
# but WITHOUT ANY WARRANTY; without even the implied warranty of        
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         
# GNU General Public License for more details.                          
#                                                                       
# You should have received a copy of the GNU General Public License     
# along with this program; if not, write to the                         
# Free Software Foundation, Inc.,                                       
# 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             
#=======================================================================

# Extract data listing from the disk_report.py output file
# Cormac Reynolds: 2016 April 6 (original version)
import json
import optparse

usage = '''%prog <expname> <disk.json>
will search <disk.json> for directories matching <expname>'''

parser = optparse.OptionParser(usage=usage, version='%prog ' + '1.0')
(options, args) = parser.parse_args()

expname = args[0]
disk_report = json.load(open(args[1]))

datasum = 0
for machine in disk_report.keys():
    print 'Disk report for', machine
    for data_area in disk_report[machine].keys():
        #print data_area
        for directory in disk_report[machine][data_area]['du']:
            if expname in directory[1]:
                print directory[0], '\t', directory[1]
                datasum += int(directory[0])

print 'total=', datasum, 'GB'
        
